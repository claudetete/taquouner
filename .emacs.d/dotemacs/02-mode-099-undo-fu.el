;;; 02-mode-099-undo-fu.el --- configuration of undo fu mode

;; Copyright (c) 2020 Claude Tete
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
;; Created: February 2020
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.replace the undo built in function without same problem than undo-tree]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package undo-fu
  :bind
  ("C-_"   . undo-fu-only-undo)
  ("M-_"   . undo-fu-only-redo)
  ("C-M-_" . undo-fu-only-redo-all)
  ) ;; (use-package undo-fu


(provide '02-mode-099-undo-fu)

;;; 02-mode-099-undo-fu.el ends here
