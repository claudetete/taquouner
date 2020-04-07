;;; 02-mode-095-ialign.el --- configuration of ialign mode

;; Copyright (c) 2019-2020 Claude Tete
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
;; Created: March 2019
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.ialign package]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package ialign
  :bind
  ;; Usage (from README)
  ;; To use it, mark a region and then call ialign. You can enter a regexp
  ;; in the minibuffer that will be passed to align-regexp command. As the
  ;; contents of minibuffer change, the region is realigned. You can also
  ;; specify other arguments to align-regexp:
  ;;
  ;; * Increment/decrement spacing (padding) with C-c - and C-c +
  ;; * Increment/decrement the parenthesis group which will be modified with
  ;;   C-c [ and C-c ] Negative parenthesis group means justify (prepend
  ;;   space to each group).
  ;; * Repeat the alignment throughout the line with C-c C-r.
  ;; * Toggle tabs with C-c C-t.
  ;; * Toggle case sensitivity with C-c M-c.
  ;; * Go to next/previous history element with M-n and M-p.
  ("C-c a d" . ialign)
  ) ;; (use-package ialign


(provide '02-mode-095-ialign)

;;; 02-mode-095-ialign.el ends here
