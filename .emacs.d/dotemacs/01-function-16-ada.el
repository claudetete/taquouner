;;; 01-function-16-ada.el --- functions to have multiple compile

;; Copyright (c) 2017-2019 Claude Tete
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

(defgroup ada-gps-command nil
  "Ada command more than just compile mode like gps"
  :group 'tools
  :group 'convenience)

(defcustom ada-gps-before-build-all nil
  "Hooks called before build all."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

(defcustom ada-gps-before-build nil
  "Hooks called before build a single file."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

(defcustom ada-gps-before-rebuild-all nil
  "Hooks called before rebuild all."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

(defcustom ada-gps-before-check nil
  "Hooks called before check."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

(defcustom ada-gps-before-pretty-print nil
  "Hooks called before pretty print."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

(defcustom ada-gps-before-clean nil
  "Hooks called before clean all."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

(defcustom ada-gps-before-build-native nil
  "Hooks called before build native dll."
  :type 'hook
  :group 'ada-gps-command
  :type 'hook)

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
    (run-hooks 'ada-gps-before-build)
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
    (run-hooks 'ada-gps-before-check)
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
    (run-hooks 'ada-gps-before-pretty-print)
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
    (run-hooks 'ada-gps-before-build-all)
    (tqnr-compile-new tqnr-ada-gps-build-all-command tqnr-ada-gps-build-all-buffer-name nil)
    ))

;; CLEAN
(defun ada-gps-clean ()
  "Clean with `ada-gps-clean-command' (not eval) in `ada-gps-clean-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.CLEAN]
    ;; [VARCOMMENT.Command of clean all generated file]
    ;; [VARIABLE.tqnr-ada-gps-clean-command "gnat clean"]
    ;; [VARCOMMENT.Buffer name of clean all generated command]
    ;; [VARIABLE.tqnr-ada-gps-clean-buffer-name "*ada-clean*"]
    (run-hooks 'ada-gps-before-clean)
    (tqnr-compile-new tqnr-ada-gps-clean-command tqnr-ada-gps-clean-buffer-name nil)))

;; REBUILD ALL
(defun ada-gps-rebuild-all ()
  "Rebuild all with `ada-gps-rebuild-all-command' (not eval) in `ada-gps-rebuild-all-buffer-name'."
  (interactive)
  (let ()
    ;; [COMMENT.REBUILD ALL]
    ;; [VARCOMMENT.Command of rebuild all generated file]
    ;; [VARIABLE.tqnr-ada-gps-rebuild-all-command "gnat clean && gnat make"]
    ;; [VARCOMMENT.Buffer name of rebuild all generated command]
    ;; [VARIABLE.tqnr-ada-gps-rebuild-all-buffer-name "*ada-rebuild-all*"]
    (run-hooks 'ada-gps-before-rebuild-all)
    (tqnr-compile-new tqnr-ada-gps-rebuild-all-command tqnr-ada-gps-rebuild-all-buffer-name nil)))

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
    (run-hooks 'ada-gps-before-build-native)
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
    ;; (if current-file
    ;;   (compile (concat command " " (shell-quote-argument (file-name-nondirectory buffer-file-name))))
    ;;   (compile command))
    ;; reset previous setting of confirmation command and buffer name
    (setq compilation-read-command keep-read-command)
    (setq compilation-buffer-name-function keep-buffer-name)))

(defun tqnr-test-local-env-var ()
  "Test"
  (interactive)
  ;; (make-local-variable 'process-environment)
  ;; (make-local-variable 'exec-path)
  (let ((path-list))
    (setq path-list
      (list
        (concat "Y:/FMS_Tools/GNATPRO/GNATPRO_18.2/bin")
        )
      )
    (setq process-environment (copy-sequence process-environment))
    (setenv "PATH" (concat (mapconcat 'identity path-list ";") ";" (getenv "PATH")))
    (setq exec-path (append path-list exec-path))
    ;; (async-shell-command "gnat --version" "*test*")
    ;; (set-process-query-on-exit-flag (get-buffer-process my-async-buffer) nil)
    (message (getenv "PATH"))
    ;;(message exec-path)
    ;;(message exec-path)
    (tqnr-compile-new "gnat --version" "*test*")
    )
  )

(defun tav-gael-start ()
  "Run FitNesse server."
  (interactive)
  (let ()
    (tqnr-compile-new "gael"))
  )

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
        ;; clean
        (define-key ada-mode-map  (kbd "C-c n c")         'ada-gps-clean)
        ;; rebuild all
        (define-key ada-mode-map  (kbd "C-c n r")         'ada-gps-rebuild-all)
        ;; build native
        (define-key ada-mode-map  (kbd "C-c n n")         'ada-gps-build-native)
        ) ;; (with-eval-after-load "ada-mode"
      ) ;; (when (and tqnr-section-mode-ada tqnr-section-mode-hydra-ada)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-16-ada)

;;; 01-function-16-ada.el ends here
