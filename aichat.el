;;; aichat.el --- AI chat integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; Homepage: https://github.com/chmouel/aichat.el

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides integration with aichat CLI tool.
;; It offers two main functions:
;; - `aichat-execute': Execute aichat and display output in a new buffer
;; - `aichat-insert': Execute aichat and insert output at point

;;; Code:

(defgroup aichat nil
  "aichat integration."
  :group 'external
  :prefix "aichat-")

(defcustom aichat-command "aichat"
  "Command to run aichat."
  :type 'string
  :group 'aichat)

(defcustom aichat-default-model "gemini:gemini-2.5-flash-lite"
  "Default model to use for aichat."
  :type 'string
  :group 'aichat)

(defcustom aichat-default-role nil
  "Default role to use for aichat.
If nil, no role argument is passed."
  :type '(choice (const :tag "No role" nil)
                 (string :tag "Role name"))
  :group 'aichat)

(defcustom aichat-gitcommit-role "gitcommit"
  "Default role to use for git commit messages when using the `aichat-gitcommit` function'.")

(defcustom aichat-config-file "~/.config/aichat/config.yaml"
  "Path to aichat configuration file."
  :type 'string
  :group 'aichat)

(defcustom aichat-strip-code-blocks t
  "Whether to strip markdown code blocks from output."
  :type 'boolean
  :group 'aichat)

(defcustom aichat-gitcommit-autoformat t
  "When non-nil, automatically format the generated commit message.
This ensures:
- The first line is treated as the subject.
- Exactly one blank line follows the subject.
- The rest of the message is refilled up to the Git template marker."
  :type 'boolean
  :group 'aichat)

(defcustom aichat-output-buffer-name "*aichat-output*"
  "Name for aichat output buffer."
  :type 'string
  :group 'aichat)

(defvar aichat-process-counter 0
  "Counter for unique process names.")

(defconst aichat-gitcommit--marker-regexp
  "^# Please enter the commit message for your changes\\."
  "Regexp marking the start of the Git commit template section.")

(defun aichat--build-args (model role)
  "Build argument list for aichat command.
MODEL is the model to use, ROLE is the optional role."
  (let ((args (list "-m" model)))
    (when role
      (setq args (append args (list "-r" role))))
    args))

(defun aichat--clean-output (output)
  "Clean OUTPUT by optionally stripping code blocks."
  (if aichat-strip-code-blocks
      (string-trim
       (replace-regexp-in-string
        "\\`[\n\r \t]*```\\(?:[a-zA-Z]*\\)?[\n\r]+\\|```[\n\r \t]*\\'"
        "" output))
    (string-trim output)))

(defun aichat--get-unique-process-name ()
  "Generate a unique process name."
  (setq aichat-process-counter (1+ aichat-process-counter))
  (format "aichat-async-%d" aichat-process-counter))

(defun aichat--create-process (input model role callback)
  "Create aichat process with INPUT, MODEL, ROLE and CALLBACK.
CALLBACK is called with cleaned output when process finishes."
  (let* ((process-name (aichat--get-unique-process-name))
         (process-buffer (generate-new-buffer
                          (format " *aichat-temp-%s*" process-name)))
         (args (aichat--build-args model role)))
    (set-process-sentinel
     (make-process
      :name process-name
      :buffer process-buffer
      :command (cons aichat-command args)
      :coding 'utf-8
      :connection-type 'pipe
      :noquery t)
     `(lambda (proc event)
        (when (string= event "finished\n")
          (when (buffer-live-p ,process-buffer)
            (let ((output (with-current-buffer ,process-buffer
                            (buffer-string))))
              (kill-buffer ,process-buffer)
              (funcall ,callback (aichat--clean-output output)))))))

    (process-send-string (get-process process-name) input)
    (process-send-eof (get-process process-name))
    (message "aichat request sent...")))

;;;###autoload
(defcustom aichat-gitcommit-fill-column 72
  "Fill column used when reflowing commit bodies."
  :type 'integer
  :group 'aichat)

(defconst aichat-gitcommit--marker-regexp
  "^# Please enter the commit message for your changes\\."
  "Regexp marking the start of the Git commit template section.")

;;;###autoload
(defun aichat-execute (&optional model role)
  "Execute aichat on buffer content and display output in new buffer.
Uses region if active, otherwise entire buffer.
MODEL defaults to `aichat-default-model'.
ROLE defaults to `aichat-default-role'."
  (interactive)
  (let* ((input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string)))
         (model (or model aichat-default-model))
         (role (or role aichat-default-role)))

    (when (string-empty-p (string-trim input))
      (user-error "No input to send to aichat"))

    (aichat--create-process
     input model role
     (lambda (output)
       (let ((output-buffer (get-buffer-create aichat-output-buffer-name)))
         (with-current-buffer output-buffer
           (erase-buffer)
           (insert output)
           (goto-char (point-min)))
         (display-buffer output-buffer)
         (message "aichat completed"))))))

;;;###autoload
(defun aichat-insert (&optional model role)
  "Execute aichat on buffer content and insert output at point.
Uses region if active, otherwise entire buffer.
MODEL defaults to `aichat-default-model'.
ROLE defaults to `aichat-default-role'."
  (interactive)
  (let* ((input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string)))
         (model (or model aichat-default-model))
         (role (or role aichat-default-role))
         (target-buffer-name (buffer-name))
         (insert-point (point)))

    (when (string-empty-p (string-trim input))
      (user-error "No input to send to aichat"))

    (aichat--create-process
     input model role
     `(lambda (output)
        (let ((buf (get-buffer ,target-buffer-name)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (save-excursion
                (goto-char ,insert-point)
                (insert output))))
          (message "aichat completed and inserted"))))))

;;;###autoload
(defun aichat-gitcommit-format-buffer ()
  "Format current buffer as a conventional commit message.

Rules:
1) Line 1 is the subject.
2) Ensure exactly one blank line after subject.
3) Refill the body up to the Git template marker.
4) Bulleted/numbered lists are refilled per-item, not merged.
5) Indented/code blocks (>=4 spaces or a tab) are left untouched."
  (interactive)
  (save-excursion
    ;; Ensure exactly one blank line after subject
    (goto-char (point-min))
    (end-of-line)
    (let ((pos-after-subject (point)))
      ;; Collapse any number of blank lines after subject
      (while (and (not (eobp))
                  (save-excursion
                    (forward-line 1)
                    (beginning-of-line)
                    (looking-at-p "^[ \t]*$")))
        (forward-line 1))
      (goto-char pos-after-subject)
      (forward-line 1)
      (unless (looking-at-p "^[ \t]*$")
        (open-line 1)))

    (let* ((fill-column aichat-gitcommit-fill-column)
           (beg (progn (goto-char (point-min))
                       (forward-line 2)            ; start of line 3
                       (point)))
           (end (or (save-excursion
                      (goto-char beg)
                      (when (re-search-forward aichat-gitcommit--marker-regexp nil t)
                        (match-beginning 0)))
                    (point-max)))
           ;; Recognize bullets: -, +, *, 1., 1), with leading indent
           (bullet-re "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+")
           ;; Treat code/indented blocks as their own “paragraphs” we won't fill
           (code-re "^[ \t]\\{4,\\}\\|^\t+")
           ;; Extend paragraph vars so each bullet item is a paragraph start,
           ;; and blank lines or code blocks separate paragraphs.
           (paragraph-start (concat "\\(?:" paragraph-start "\\)\\|"
                                    bullet-re "\\|" code-re))
           (paragraph-separate (concat "\\(?:" paragraph-separate "\\)\\|"
                                       bullet-re "\\|" code-re))
           ;; Help Emacs detect the bullet/indent as a fill prefix for wrapping
           (adaptive-fill-regexp (concat "\\(?:" adaptive-fill-regexp "\\)\\|"
                                         "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+"))
           (use-hard-newlines nil))
      (when (< beg end)
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((pstart (point)))
              ;; If current line is a code block, skip it verbatim
              (cond
               ((looking-at-p code-re)
                (forward-line 1))                ; leave code line untouched
               (t
                ;; Move to end of paragraph using our custom paragraph rules
                (forward-paragraph)
                (let ((pend (point)))
                  ;; Only fill if the paragraph contains non-blank, non-code text
                  (save-excursion
                    (goto-char pstart)
                    (unless (or (>= pstart pend)
                                (looking-at-p "^[ \t]*$")
                                (looking-at-p code-re))
                      ;; Fill this paragraph; bullets are preserved due to
                      ;; paragraph-start and adaptive-fill-regexp.
                      (fill-region pstart pend)))))
               ;; Skip consecutive blank lines cleanly
               (skip-chars-forward "\n")))))))))

;;;###autoload
(defun aichat-gitcommit (&optional role)
  "Execute aichat for git commit and insert at beginning of buffer.
ROLE defaults to `aichat-gitcommit-role'. If
`aichat-gitcommit-autoformat' is non-nil, run
`aichat-gitcommit-format-buffer' afterward."
  (interactive)
  (let* ((input (buffer-string))
         (target-buffer-name (buffer-name))
         (commit-role (or role aichat-gitcommit-role)))
    (when (string-empty-p (string-trim input))
      (user-error "No input to send to aichat"))
    (aichat--create-process
     input aichat-default-model commit-role
     `(lambda (output)
        (let ((buf (get-buffer ,target-buffer-name)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                (insert output "\n")
                (when aichat-gitcommit-autoformat
                  (aichat-gitcommit-format-buffer))))
            (message "Git commit message inserted")))))))


;;;###autoload
(defun aichat--parse-yaml-models (config-file)
  "Parse aichat config file and extract all available models.
Returns a list of model names in the format 'client-name:model-name'."
  (when (file-exists-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file)
      (let ((models '())
            (current-client nil)
            (in-clients nil)
            (in-models nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (thing-at-point 'line t)))
            (cond
             ((string-match "^clients:" line)
              (setq in-clients t))
             ((and in-clients (string-match "^- type:" line))
              (setq in-models nil)
              (setq current-client nil))
             ((and in-clients (string-match "^  name: \\(.+\\)" line))
              (setq current-client (string-trim (match-string 1 line))))
             ((and in-clients current-client (string-match "^  models:" line))
              (setq in-models t))
             ((and in-models (string-match "^  - name: \\(.+\\)" line))
              (let ((model-name (string-trim (match-string 1 line))))
                (push (format "%s:%s" current-client model-name) models)))
             ((and in-clients (string-match "^[a-zA-Z]" line))
              (setq in-clients nil))))
          (forward-line 1))
        (nreverse models)))))

;;;###autoload
(defun aichat-set-model ()
  "Set the aichat default model by selecting from available models in config."
  (interactive)
  (let* ((config-file (expand-file-name aichat-config-file))
         (models (aichat--parse-yaml-models config-file)))
    (if models
        (let ((selected-model (completing-read "Select model: " models nil t)))
          (setq aichat-default-model selected-model)
          (message "aichat default model set to: %s" selected-model))
      (user-error "No models found in config file: %s" config-file))))

(provide 'aichat)

;;; aichat.el ends here
