;;; 02-mode-087-ada.el --- configuration of ada mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2020 Claude Tete
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
;; Created: October 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Ada mode for edit/navigate/compile ada source code]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package ada-mode
  ;; make sure it is loaded and custom without searched in package list
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins"))

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

  (defun my-ada-mode-hook ()
    ;; fill-column is a automatically local variable when set
    (set-fill-column 100)
    ;; tab-width is a automatically local variable when set
    (setq tab-width 7)
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
  ) ;; (use-package ada-mode


;; see shortcut for build/check/clean in 01-function-16-ada.el no other function are currently customized


(provide '02-mode-087-ada)

;;; 02-mode-087-ada.el ends here
