;;; 02-mode-087-ada.el --- configuration of ada mode

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
  ;; Mode hooks execute before local variables are processed, so your hooks cannot respond to local vars at that point in time
  ;; Use the regular major mode hook to add a buffer-local hack-local-variables-hook (https://www.emacswiki.org/emacs/LocalVariables)
  :hook
  (ada-mode-hook . my-ada-mode-hook)

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
  ;; indent broken line with 3 spaces
  (ada-indent-broken 3)
  ;; indent comment following gnat rules
  (ada-indent-comment-gnat t)
  ;; indent broken line relative to start of expression (t) or previous line (nil)
  (ada-indent-hanging-rel-exp nil)
  ;; indentation of record
  (ada-indent-record-rel-type 3)
  ) ;; (use-package ada-mode


(use-package ada-ref-man)

;; see shortcut for build/check/clean in 01-function-16-ada.el no other function are currently customized


(provide '02-mode-087-ada)

;;; 02-mode-087-ada.el ends here
