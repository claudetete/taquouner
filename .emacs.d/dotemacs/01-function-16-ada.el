;;; 01-function-16-ada.el --- functions to have multiple compile -*- lexical-binding: t -*-

;; Copyright (c) 2017-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
;; Version: 0.3
;; Created: November 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; subsection comment
;; [SUBHEADER.functions to have multiple compile mode in ada]
;; [SUBDEFAULT.nil]
;;
;; TODO: use local environnment to run this commands (see make-local-variable
;; process-environment)


;;; Code:
;;

(defgroup ada-command nil
  "Ada command more than just compile mode like gps"
  :group 'tools
  :group 'convenience)

(defcustom ada-compile-hook-before nil
  "Hooks called before compiling a single file."
  :type 'hook
  :group 'ada-command
  :type 'hook)

(defcustom ada-check-hook-before nil
  "Hooks called before checking a single file."
  :type 'hook
  :group 'ada-command
  :type 'hook)

(defcustom ada-pretty-print-hook-before nil
  "Hooks called before pretty print a single file."
  :type 'hook
  :group 'ada-command
  :type 'hook)

;; contains last showed buffer
(defvar ada-command-buffer-name-last "")

;; COMPILE
(defun ada-compile ()
  "Compile current file with `tqnr-ada-compile-command' (not eval) in `tqnr-ada-compile-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.COMPILATION]
    ;; [VARCOMMENT.Command to compile one file in ada]
    ;; [VARIABLE.tqnr-ada-compile-command "gnat build"]
    ;; [VARCOMMENT.Buffer name of build one file in ada]
    ;; [VARIABLE.tqnr-ada-compile-buffer-name "*ada-compilation*"]
    (run-hooks 'ada-compile-hook-before)
    (tqnr-compile-new tqnr-ada-compile-command tqnr-ada-compile-buffer-name t)))

;; CHECK
(defun ada-check ()
  "Check current file with `tqnr-ada-check-command' (not eval) in `tqnr-ada-check-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.CHECK]
    ;; [VARCOMMENT.Command to check one file in ada]
    ;; [VARIABLE.tqnr-ada-check-command "gnat check"]
    ;; [VARCOMMENT.Buffer name of check one file in ada]
    ;; [VARIABLE.tqnr-ada-check-buffer-name "*ada-check*"]
    (run-hooks 'ada-check-hook-before)
    (tqnr-compile-new tqnr-ada-check-command tqnr-ada-check-buffer-name t)))

;; PRETTY PRINT
(defun ada-pretty-print ()
  "Pretty print current file with `tqnr-ada-pretty-print-command' (not eval) in `tqnr-ada-pretty-print-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.PRETTY PRINT]
    ;; [VARCOMMENT.Command to pretty-print one file in ada]
    ;; [VARIABLE.tqnr-ada-pretty-print-command "gnat pretty"]
    ;; [VARCOMMENT.Buffer name of pretty-print one file in ada]
    ;; [VARIABLE.tqnr-ada-pretty-print-buffer-name "*ada-pretty-print*"]
    (run-hooks 'ada-pretty-print-hook-before)
    (tqnr-compile-new tqnr-ada-pretty-print-command tqnr-ada-pretty-print-buffer-name t)))

(defun tqnr-compile-new (command buffer-name &optional current-file)
  "Call compile with new BUFFER-NAME and specific COMMAND which can be
optionally suffix with CURRENT-FILE."
  (let ((keep-read-command compilation-read-command)
         (keep-buffer-name compilation-buffer-name-function))
    (setq ada-command-buffer-name-last buffer-name)
    ;; do not ask confirmation of command
    (setq compilation-read-command nil)
    ;; redefine compilation-buffer-name-function just for calling compile to set buffer name
    (setq compilation-buffer-name-function (lambda (mode) buffer-name))
    ;; call compile mode with current command and file
    (if current-file
      (compile (concat command " " (shell-quote-argument (file-name-nondirectory buffer-file-name))))
      (compile command))
    ;; reset previous setting of confirmation command and buffer name
    (setq compilation-read-command keep-read-command)
    (setq compilation-buffer-name-function keep-buffer-name)))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()

    (when (and tqnr-section-mode-ada tqnr-section-mode-hydra-ada)
      (with-eval-after-load "ada-mode"
        ;; compile
        (define-key ada-mode-map        (kbd "<M-f10>")       'ada-compile)
        ;; check
        (define-key ada-mode-map        (kbd "<M-S-f10>")     'ada-check)
        ;; pretty print
        (define-key ada-mode-map        (kbd "<S-f10>")       'ada-pretty-print)
        ) ;; (with-eval-after-load "ada-mode"
      ) ;; (when (and tqnr-section-mode-ada tqnr-section-mode-hydra-ada)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-16-ada)

;;; 01-function-16-ada.el ends here
