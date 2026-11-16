;;; 02-mode-113-consult.el --- configuration of consult -*- lexical-binding: t -*-

;; Copyright (c) 2023 Claude Tete
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
;; Version: 0.1
;; Created: March 2023
;; Last-Updated: March 2023

;;; Commentary:
;;
;; [SUBHEADER.consult: command with filtering like completions]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package consult
  :ensure t

  :bind
  ("C-x C-b"        . consult-buffer)
  ("M-\\"           . consult-imenu)
  ("C-M-Y"          . consult-register)
  ("C-S-M-<insert>" . consult-register)
  ("M-Y"            . consult-register-load)
  ("C-S-<insert>"   . consult-register-load)
  ("M-W"            . consult-register-store)
  ("<M-f3>"         . consult-ripgrep-dir)

  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult--grep consult--ripgrep-make-builder
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :group nil)

  (defun consult-ripgrep-dir ()
    "Run `consult-ripgrep' on a selected DIR"
    (interactive)
    (let ((string (if (use-region-p)
                      (thing-at-point 'region)
                    (thing-at-point 'symbol)))
          (dir (list (read-directory-name "Run consult-ripgrep in directory: " nil "" t))))
      (consult-ripgrep dir string)))

  ;; Plug the consult with `complete-symbol`
  (setq completion-in-region-function #'consult-completion-in-region)

  ;; from Gemini answer
  (defun consult-completion-in-region-one-line (orig-fun &rest args)
    "Force strict one-line candidates in `consult-completion-in-region'."
    (let ((completions-detailed nil)
          (completion-extra-properties
           (plist-put
            (plist-put (copy-sequence completion-extra-properties)
                       :annotation-function nil)
            :affixation-function nil)))
      (apply orig-fun args)))

  (advice-add 'consult-completion-in-region :around #'consult-completion-in-region-one-line)
  )

;; (use-package consult-projectile
;;   :ensure t)

(use-package consult-eglot
  :ensure t
  :config
  ;; use consult for xref buffer
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  )

;; (use-package consult-xref-stack
;;   :vc
;;   (:url "https://github.com/brett-lempereur/consult-xref-stack" :branch "main")
;;   :bind
;;   (("C-," . consult-xref-stack-backward)))

(use-package consult-compile-command
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/"))

  :config
  ;; [VARCOMMENT.configurate the consult-compile-command package]
  ;; [VARIABLE.consult-compile-command-configuration 'consult-compile-command-configuration-default]
  (defun consult-compile-command-configuration-default ()
    )
  (funcall consult-compile-command-configuration)

  ;; stop scrolling compilation at first error
  (setq compilation-scroll-output 'first-error)
  )


(provide '02-mode-113-consult)

;;; 02-mode-113-consult.el ends here
