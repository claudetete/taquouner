;;; 02-mode-120-vundo.el --- configuration of vundo -*- lexical-binding: t -*-

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
;; Created: June 2023
;; Last-Updated: June 2023

;;; Commentary:
;;
;; [SUBHEADER.treemacs: display file tree on one side]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package vundo
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)
  ;; use unicode instead of ascii characters to display the tree
  (setq vundo-glyph-alist vundo-unicode-symbols)

  :bind
  ("s-_" . vundo))

(provide '02-mode-120-vundo)

;;; 02-mode-120-vundo.el ends here
