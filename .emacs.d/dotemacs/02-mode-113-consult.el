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
  ("C-x C-b" . consult-buffer)
  ("M-\\"    . consult-imenu)

  :config
  (defun consult-ripgrep-dwim ()
    (interactive)
    (consult-ripgrep (file-name-directory buffer-file-name)))
  )

;; (use-package consult-projectile
;;   :ensure t)

(use-package consult-eglot
  :ensure t)

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
