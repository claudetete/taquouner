;;; 02-mode-087-ada.el --- configuration of ada mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2021 Claude TETE
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
;; Version: 0.4
;; Created: October 2017
;; Last-Updated: January 2021

;;; Commentary:
;;
;; [SUBHEADER.Ada mode for edit/navigate/compile ada source code]
;; [SUBDEFAULT.nil]


;;; Code:

;; duplicate otherwise ada-mode is not loaded
(use-package ada-mode
  ;; make sure it is loaded and custom without searched in package list
  ;; :ensure nil
  ;; :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/"))
  )

(use-package ada-mode
  ;; make sure it is loaded and custom without searched in package list
  ;; :ensure nil
  ;; :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/"))
  ;;:mode "\\.gpr\\'"

  ;; Mode hooks execute before local variables are processed, so your hooks cannot respond to local vars at that point in time
  ;; Use the regular major mode hook to add a buffer-local hack-local-variables-hook (https://www.emacswiki.org/emacs/LocalVariables)
  :hook
  (ada-mode . my-ada-mode-hook)

  :bind (:map ada-mode-map
          ;; move
          ("<C-S-up>"   . backward-sexp)
          ("<C-S-down>" . forward-sexp))

  :config
  (setq ada-build-run-cmd "./${main}.exe")

  (defun ada-mode-after-direnv ()
    (lsp-register-custom-settings
      '(("ada.scenarioVariables.BINUTILS_SRC_DIR" "/null")
         ("ada.scenarioVariables.BUILD_TYPE" project-build-type "Debug")))
    (setq lsp-ada-project-file (getenv "LSP_ADA_PROJECT_FILE"))
    (lsp-mode t))

  (defun my-ada-mode-hook ()
    ;; fill-column is a automatically local variable when set
    (set-fill-column 150)
    ;; tab-width is a automatically local variable when set
    (setq tab-width 7)

    (when (and tqnr-section-mode-lsp-mode tqnr-section-mode-projectile-direnv)
      (add-hook 'projectile-direnv-after-export-hook 'ada-mode-after-direnv nil t))
    )

  :custom
  ;; indentation is 3 spaces
  (ada-indent 3)
  ;; indent broken declaration
  ;;     declare
  ;;        A,
  ;;        >>>>>B : Integer;
  (ada-broken-decl-indent 0)
  ;; indent broken line with 3 spaces
  ;;     My_Var : My_Type := (Field1 =>
  ;;                     >>>>>>>>>Value);
  (ada-broken-indent 3)
  ;; indent broken parenthesis with 0 space
  ;;        Func (Param1,
  ;;          >>>>>Param2);
  (ada-continuation-indent 0)
  ;; indent comment following gnat rules
  (ada-indent-comment-gnat t)
  ;; comment prefix
  (ada-fill-comment-prefix "-- ")
  ;; indent broken line relative to start of expression (t) or previous line (nil)
  (ada-indent-hanging-rel-exp nil)
  ;; indentation of record
  (ada-indent-record-rel-type 3)
  ;; indentation of rename
  (ada-indent-renames 3)
  ;; ada version
  (ada-language-version 'ada2005)
  ;; ada-move-to-start move to sub declaration instead of 'begin'
  (ada-move-to-declaration t)

  ;; (ada-diagnostics-backend 'none)
  ;; (ada-face-backend 'none)
  ;; (ada-indent-backend 'none)
  ;; (ada-statement-backend 'none)
  (ada-xref-backend 'eglot)
  ) ;; (use-package ada-mode


;; see shortcut for build/check/clean in 01-function-16-ada.el no other function are currently customized


(provide '02-mode-087-ada)

;;; 02-mode-087-ada.el ends here
