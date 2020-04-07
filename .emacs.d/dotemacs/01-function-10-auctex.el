;;; 01-function-10-auctex.el --- add some function about auctex -*- lexical-binding: t -*-

;; Copyright (c) 2017-2019 Claude Tete
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
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about AUCTeX]
;; [SUBDEFAULT.nil]
;;


;;; Code:

(when tqnr-section-mode-auctex
  ;;; save the current buffer/document and compile it in auctex
  (defun auctex-save-and-compile ()
    (interactive)
    (save-buffer)
    (TeX-save-document "")
    (TeX-command "LaTeX" 'TeX-active-master 0))
  )


(provide '01-function-10-auctex)

;;; 01-function-10-auctex.el ends here
