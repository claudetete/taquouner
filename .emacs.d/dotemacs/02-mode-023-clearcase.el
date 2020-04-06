;;; 02-mode-023-clearcase.el --- configuration of clearcase mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.ClearCase mode]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package clearcase
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/clearcase.el"))

  :bind
  ;; ediff with previous (can open a new frame before with C-x 5 2)
  ("C-x v `" . clearcase-ediff-pred-current-buffer)
  ;; ediff with named version
  ("C-x v 1" . clearcase-ediff-named-version-current-buffer)
  ) ;; (use-package  clearcase

;; toggle compile window when quit config spec mode
(when tqnr-section-mode-cedet-ecb
  (use-package clearcase
    :hook
    (clearcase-config-spec-quit-hook . ecb-toggle-compile)))


(provide '02-mode-023-clearcase)

;;; 02-mode-023-clearcase.el ends here
