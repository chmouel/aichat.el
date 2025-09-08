# emacs-aichat

Emacs integration for the [aichat](https://github.com/sigoden/aichat) CLI tool.
Send buffer content or regions to AI models and handle responses
asynchronously.

# Screenshot (auto generate git commit message via aichat)

https://github.com/user-attachments/assets/5b05d265-0ca3-4503-a6de-cb97909b3928

## Features

- Execute aichat and display output in a dedicated buffer
- Insert AI responses directly at point
- Special git commit message generation
- Async execution (non-blocking)
- Configurable models and roles
- Auto-cleanup of markdown code blocks

## Installation

Download `aichat.el` and add to your load path:

```elisp
(require 'aichat)
```

Or with `use-package`:

```elisp
(use-package aichat
  :custom
  (aichat-default-model "gemini:gemini-2.5-flash") ;; Make sure to set your model to what you have configured in aichat
  :load-path "path/to/aichat.el"
  :bind (("C-c a e" . aichat-execute)
         ("C-c a i" . aichat-insert)
         ("C-c a g" . aichat-gitcommit)))
```

For Emacs 29+ you can use use-package-vc or for Emacs 30:

```elisp
(use-package aichat
  :custom
  (aichat-default-model "gemini:gemini-2.5-flash") ;; Make sure to set your model to what you have configured in aichat
  :vc (:url "https://github.com/chmouel/aichat.el/" :rev :newest)
  :bind (("C-c a e" . aichat-execute)
         ("C-c a i" . aichat-insert)
         ("C-c a g" . aichat-gitcommit))))
```

if you want to automatically generate a commit message when creating a commit
message with `git-commit-mode` you can do this:

```elisp
(use-package aichat
  :defer t
  :custom
  (aichat-default-model "gemini:gemini-2.5-flash") ;; Make sure to set your model to what you have configured in aichat
  :after git-commit
  :vc (:url "https://github.com/chmouel/aichat.el/" :rev :newest)
  :hook
  (git-commit-mode . my-aichat-commit-mode)
  :config
  (defun my-aichat-commit-mode ()
    "Setup GIT Commit mode."
    (aichat-gitcommit)
    (message "GPTel Commit mode activated")
    (local-set-key (kbd "C-c a") #'aichat-gitcommit)))
```

> ℹ️ **Info**
> You’ll need to create the `gitcommit` role (or whatever you have set as
> `aichat-gitcommit-role`) in `~/.config/aichat/roles/`. For reference, you can
> check my own [gitcommit.md](./prompts/gitcommit.md).

## Configuration

Use `M-x customize-group RET aichat RET` to customize options.

Make sure to set your default model and roles as needed in your Emacs config.

### Available Options

- `aichat-command` - Path to aichat binary (default: "aichat")
- `aichat-default-model` - Default model string
- `aichat-default-role` - Default role or nil
- `aichat-gitcommit-role` - Role for git commit generation
- `aichat-strip-code-blocks` - Remove markdown code blocks from output
- `aichat-output-buffer-name` - Name for output buffer

### Requirements

Requires the [aichat](https://github.com/sigoden/aichat) CLI tool.

## Usage

### Functions

- `aichat-execute` - Run aichat and show output in `*aichat-output*` buffer
- `aichat-insert` - Run aichat and insert output at point
- `aichat-gitcommit` - Generate commit message and insert at buffer beginning

All functions operate on the active region if present, otherwise the entire buffer.

### Examples

```elisp
;; Basic usage with defaults
(aichat-execute)

;; Override model and role
(aichat-execute "claude:claude-3-sonnet" "editor")

;; Generate git commit message
(aichat-gitcommit)
```

## Author

Chmouel Boudjnah <chmouel@chmouel.com>

## License

GPL-3.0
