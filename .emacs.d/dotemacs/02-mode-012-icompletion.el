;;; 02-mode-012-icompletion.el --- configuration of icompletion mode

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
;; [SUBHEADER.more completion in minibuffer]
;; [SUBDEFAULT.nil]


;;; Code:
;; Completion in Minibuffer
(use-package icomplete
  ;; make sure it is loaded and custom without searched in package list
  ;; it is not listed as built-in package
  :ensure nil

  :init
  (icomplete-mode t))


(provide '02-mode-012-icompletion)

;;; 02-mode-012-icompletion.el ends here
