;;; aichat.el --- AI chat integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Version: 0.1.0
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

(defcustom aichat-strip-code-blocks t
  "Whether to strip markdown code blocks from output."
  :type 'boolean
  :group 'aichat)

(defcustom aichat-output-buffer-name "*aichat-output*"
  "Name for aichat output buffer."
  :type 'string
  :group 'aichat)

(defvar aichat-process-counter 0
  "Counter for unique process names.")

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
(defun aichat-gitcommit (&optional role)
  "Execute aichat for git commit and insert at beginning of buffer.
ROLE defaults to `aichat-gitcommit-role'."
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
                (insert output "\n"))))
          (message "Git commit message inserted"))))))

(provide 'aichat)

;;; aichat.el ends here
