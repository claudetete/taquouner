;;; 02-mode-060-ediff.el --- configuration of ediff mode -*- lexical-binding: t -*-

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
;; [SUBHEADER.graphical diff (## to toggle whitespace ignoring)]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package ediff
  :bind (:map ediff-mode-map
          ;; previous diff
          ("<M-up>"    . ediff-previous-difference)
          ;; next diff
          ("<M-down>"  . ediff-next-difference)
          ;; get modification from left
          ("<M-right>" . ediff-copy-A-to-B)
          ;; get modification from right
          ("<M-left>"  . ediff-copy-B-to-A)
          )
  :config
  ;; always split with two vertical buffer in ediff mode
  ;;(add-hook 'ediff-before-setup-hook 'new-frame)
  ;;(add-hook 'ediff-quit-hook 'delete-frame)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  ) ;; (use-package ediff


(provide '02-mode-060-ediff)

;;; 02-mode-060-ediff.el ends here
