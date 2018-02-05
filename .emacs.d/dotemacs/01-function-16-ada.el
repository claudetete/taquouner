;;; 01-function-16-ada.el --- functions to have multiple compile

;; Copyright (c) 2017-2018 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.2
;; Created: November 2017
;; Last-Updated: January 2018

;;; Commentary:
;;
;; subsection comment
;; [SUBHEADER.functions to have multiple compile mode in ada]
;; [SUBDEFAULT.nil]
;;
;; TODO: use local environnment to run this commands (see make-local-variable
;; process-environment)

;;; Change Log:
;; 2018-01-31 (0.2)
;;    add buffer last name variable to be able to switch to last command
;; 2017-11-21 (0.1)
;;    create from scratch


;;; Code:
;;

;; contains last showed buffer
(defvar ada-gps-buffer-name-last "")

;; BUILD
(defun ada-gps-build ()
  "Build current file with `tqnr-ada-gps-build-command' (not eval) in `tqnr-ada-gps-build-buffer-name'."
  (interactive)
  (let ()
    ;; [[VARCOMMENT.BUILD
    ;; Command of build one file in ada
    ;; ]]
    ;; [VARIABLE.tqnr-ada-gps-build-command "gnat build"]
    ;; [VARCOMMENT.Buffer name of build one file in ada]
    ;; [VARIABLE.tqnr-ada-gps-build-buffer-name "*ada-build*"]
    (tqnr-compile-new tqnr-ada-gps-build-command tqnr-ada-gps-build-buffer-name t)))

;; CHECK
(defun ada-gps-check ()
  "Check current file with `tqnr-ada-gps-check-command' (not eval) in `tqnr-ada-gps-check-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.CHECK]
    ;; [VARCOMMENT.Command of check one file in ada]
    ;; [VARIABLE.tqnr-ada-gps-check-command "gnat check"]
    ;; [VARCOMMENT.Buffer name of check one file in ada]
    ;; [VARIABLE.tqnr-ada-gps-check-buffer-name "*ada-check*"]
    (tqnr-compile-new tqnr-ada-gps-check-command tqnr-ada-gps-check-buffer-name t)))

;; PRETTY PRINT
(defun ada-gps-pretty-print ()
  "Pretty print current file with `tqnr-ada-gps-pretty-print-command' (not eval) in `tqnr-ada-gps-pretty-print-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.PRETTY PRINT]
    ;; [VARCOMMENT.Command of pretty-print one file in ada]
    ;; [VARIABLE.tqnr-ada-gps-pretty-print-command "gnat pretty"]
    ;; [VARCOMMENT.Buffer name of pretty-print one file in ada]
    ;; [VARIABLE.tqnr-ada-gps-pretty-print-buffer-name "*ada-pretty-print*"]
    (tqnr-compile-new tqnr-ada-gps-pretty-print-command tqnr-ada-gps-pretty-print-buffer-name t)))

;; BUILD ALL
(defun ada-gps-build-all ()
  "Build all project files with `tqnr-ada-gps-build-all-command' (not eval) in `tqnr-ada-gps-build-all-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.BUILD ALL]
    ;; [VARCOMMENT.Command of build all project files]
    ;; [VARIABLE.tqnr-ada-gps-build-all-command "gnat make"]
    ;; [VARCOMMENT.Buffer name of build all project files command]
    ;; [VARIABLE.tqnr-ada-gps-build-all-buffer-name "*ada-build-all*"]
    (tqnr-compile-new tqnr-ada-gps-build-all-command tqnr-ada-gps-build-all-buffer-name nil)))

;; CLEAN ALL
(defun ada-gps-clean-all ()
  "Clean all with `ada-gps-clean-all-command' (not eval) in `ada-gps-clean-all-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.CLEAN ALL]
    ;; [VARCOMMENT.Command of clean all generated file]
    ;; [VARIABLE.tqnr-ada-gps-clean-all-command "gnat clean"]
    ;; [VARCOMMENT.Buffer name of clean all generated command]
    ;; [VARIABLE.tqnr-ada-gps-clean-all-buffer-name "*ada-clean-all*"]
    (tqnr-compile-new tqnr-ada-gps-clean-all-command tqnr-ada-gps-clean-all-buffer-name nil)))

;; BUILD DLL
(defun ada-gps-build-native ()
  "Build dll with `ada-gps-build-native-command' (not eval) in `ada-gps-build-native-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.BUILD NATIVE]
    ;; [VARCOMMENT.Command of build for native execution]
    ;; [VARIABLE.tqnr-ada-gps-build-native-command "gnat clean"]
    ;; [VARCOMMENT.Buffer name of native build command]
    ;; [VARIABLE.tqnr-ada-gps-build-native-buffer-name "*ada-build-native*"]
    (tqnr-compile-new tqnr-ada-gps-build-native-command tqnr-ada-gps-build-native-buffer-name nil)))

(defun tqnr-compile-new (command buffer-name &optional current-file)
  "Call compile with new BUFFER-NAME and specific COMMAND which can be
optionally suffix with CURRENT-FILE."
  (let ((keep-read-command compilation-read-command)
         (keep-buffer-name compilation-buffer-name-function))
    (setq ada-gps-buffer-name-last buffer-name)
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
        ;; build
        (define-key ada-mode-map  (kbd "<f10>")           'ada-gps-build)
        ;; build all
        (define-key ada-mode-map  (kbd "C-c n b")         'ada-gps-build-all)
        ;; check
        (define-key ada-mode-map  (kbd "C-c n s")         'ada-gps-check)
        ;; pretty print
        (define-key ada-mode-map  (kbd "C-c n p")         'ada-gps-pretty-print)
        ;; clean all
        (define-key ada-mode-map  (kbd "C-c n c")         'ada-gps-clean-all)
        ;; build native
        (define-key ada-mode-map  (kbd "C-c n n")         'ada-gps-build-native)
        ) ;; (with-eval-after-load "ada-mode"
      ) ;; (when (and tqnr-section-mode-ada tqnr-section-mode-hydra-ada)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-16-ada)

;;; 01-function-16-ada.el ends here
