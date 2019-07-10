;;; shadowenv.el --- Shadowenv integration. -*- lexical-binding: t; -*-

;; Author: Dante Catalfamo <dante.catalfamo@shopify.com>
;; Version: 0.9.1
;; Package-Requires: ((emacs "24"))
;; Keywords: shadowenv, tools
;; URL: https://github.com/Shopify/shadowenv.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides integration with shadowenv environment shadowing.
;; Lists the number of shadowed environment variables in the mode line.
;; See https://shopify.github.io/shadowenv/ for more details.

;;; Commands

;; `shadowenv-mode' Toggle shadowenv mode in a buffer.
;; `shadowenv-reload' Reload shadowenv environment.
;; `shadowenv-shadows' Display changes to the current environment.

;;; use-package

;; Here's an example use-package configuration:

;; (use-package shadowenv
;;   :load-path "~/.emacs.d/shadowenv"
;;   :hook ((find-file . shadowenv-reload)
;;          (eshell-mode . shadowenv-reload)
;;          (eshell-directory-change . shadowenv-reload)
;;          (comint-mode . shadowenv-reload))
;;   :commands (shadowenv-mode))

;;; Code:

(require 'eshell)

(defconst shadowenv--instruction-split (string #x1E))
(defconst shadowenv--operand-split (string #x1F))
(defconst shadowenv--set-unexported (string #x01))
(defconst shadowenv--set-exported (string #x02))
(defconst shadowenv--unset (string #x03))
(defconst shadowenv-output-buffer "*shadowenv output*"
  "Output buffer for shadowenv command.")


(defgroup shadowenv nil
  "Shadowenv environment shadowing."
  :group 'emacs)


(defcustom shadowenv-binary-location nil
    "The location of the shadowenv binary.
If nil, binary location is determined with PATH environment variable."
  :type '(choice (const :tag "Get location from $PATH" nil)
                 (file :tag "Specify location"))
  :group 'shadowenv)


(defcustom shadowenv-lighter "Shadowenv"
  "Shadowenv mode line lighter prefix."
  :type 'string
  :group 'shadowenv)


(defvar shadowenv-data ""
  "Internal shadowenv data.")
(make-variable-buffer-local 'shadowenv-data)

(defvar shadowenv--mode-line (concat " " shadowenv-lighter)
  "Shadowenv mode line.")
(make-variable-buffer-local 'shadowenv--mode-line)

(defvar shadowenv-shadows nil
  "List of shadowed environment variables and their replacements.")
(make-variable-buffer-local 'shadowenv-shadows)

(defun shadowenv-run (data)
  "Run shadowenv porcelain with DATA."
  (unless (if shadowenv-binary-location
              (file-executable-p shadowenv-binary-location)
            (executable-find "shadowenv"))
    (error "Cannot find shadowenv binary"))

  (with-current-buffer (get-buffer-create shadowenv-output-buffer)
    (erase-buffer))

  (let ((shadowenv-binary (or shadowenv-binary-location "shadowenv"))
        (output-buffers (list shadowenv-output-buffer nil)))
    (if (eq 0 (call-process shadowenv-binary nil output-buffers nil "hook" "--porcelain" data))
        (with-current-buffer shadowenv-output-buffer
          (replace-regexp-in-string "\n$" "" (buffer-string)))
      (view-buffer-other-window shadowenv-output-buffer))))


(defun shadowenv-parse-instructions (instructions-string)
  "Parse INSTRUCTIONS-STRING returned from shadowenv."
  (save-match-data
    (let ((instructions (split-string instructions-string shadowenv--instruction-split t))
          pairs)
      (dolist (instruction instructions pairs)
        (push (split-string instruction shadowenv--operand-split) pairs))
      pairs)))


(defun shadowenv--set (instruction)
  "Set a single INSTRUCTION from shadowenv.
Instructions come in the form of (opcode variable [value])."
  (let ((opcode (car instruction))
        (variable (cadr instruction))
        (value (caddr instruction)))
    (unless (string= "__shadowenv_data" variable)
      (push (cons variable (cons (getenv variable) value)) shadowenv-shadows))
    (cond
     ((string= opcode shadowenv--set-exported)
      (setenv variable value))
     ((string= opcode shadowenv--unset)
      (setenv variable))
     ((string= opcode shadowenv--set-unexported)
      (if (string= variable "__shadowenv_data")
          (setq shadowenv-data value)
        (warn "Unrecognized operand for SET_UNEXPORTED operand: %s" variable))))))


(defun shadowenv--update-mode-line (number)
  "Update the shadowenv mode line to reflect the NUMBER of environment shadows."
  (when (< number 1)
    (setq number "-"))
  (setq shadowenv--mode-line (format " %s[%s]" shadowenv-lighter number)))


;;;###autoload
(define-minor-mode shadowenv-mode
  "Shadowenv environment shadowing."
  :init-value nil
  :lighter shadowenv--mode-line
  (if shadowenv-mode
      (shadowenv-setup)
    (shadowenv-down)))


;;;###autoload
(defun shadowenv-reload ()
  "Reload shadowenv configuration."
  (interactive)
  (shadowenv-mode -1)
  (shadowenv-mode 1))


(defun shadowenv-setup ()
  "Setup shadowenv environment."
  (unless shadowenv-mode
    (error "Shadowenv mode must be enabled first"))
  (make-local-variable 'process-environment)
  (make-local-variable 'exec-path)
  (make-local-variable 'eshell-path-env)
  (setq process-environment (copy-sequence process-environment))
  (setq exec-path (copy-sequence exec-path))
  (let* ((instructions (shadowenv-parse-instructions (shadowenv-run shadowenv-data)))
         (num-items (length instructions)))
    (mapc #'shadowenv--set instructions)
    (shadowenv--update-mode-line (1- num-items)))
  (let ((path (getenv "PATH")))
    (setq eshell-path-env path)
    (setq exec-path (parse-colon-path path))))


(defun shadowenv-down ()
  "Disable the shadowenv environment."
  (kill-local-variable 'process-environment)
  (kill-local-variable 'exec-path)
  (kill-local-variable 'eshell-path-env)
  (setq shadowenv-data "")
  (setq shadowenv-shadows nil)
  (shadowenv--update-mode-line 0))


(defun shadowenv-shadows ()
  "Display the environment shadows in a popup buffer."
  (interactive)
  (let ((shadowenv-buffer (get-buffer-create (format "*shadowenv %s*" (buffer-name))))
        (no-change (eq shadowenv-shadows nil)))
    (with-current-buffer shadowenv-buffer
      (erase-buffer)
      (when no-change
        (insert "No environment shadows in the current buffer.")))
    (dolist (shadow shadowenv-shadows)
      (let* ((variable (car shadow))
            (shadow-states (cdr shadow))
            (old-state (or (car shadow-states) ""))
            (new-state (or (cdr shadow-states) "")))
        (with-current-buffer shadowenv-buffer
          (insert variable "\n" old-state " -> " new-state "\n\n"))))
    (with-current-buffer shadowenv-buffer
      (goto-char (point-min)))
    (view-buffer-other-window shadowenv-buffer)))


(provide 'shadowenv)
;;; shadowenv.el ends here
