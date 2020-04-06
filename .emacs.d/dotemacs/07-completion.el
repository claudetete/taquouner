;;; 07-completion.el --- a config file for completion settings

;; Copyright (c) 2006-2019 Claude Tete
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
;; Version: 1.6
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [HEADER.enable letter case completion + dynamic completion]
;; [DEFAULT.t]



;;; Code:
;; completion is case sensitive
(defvar dabbrev-case-replace nil)

;;;; enable dynamic word completion (never used)
;;;; from grandm_y
;;;; Interfere with CUA selection mode (insert only null character)
;;(dynamic-completion-mode)

;; try
;; coupled with modified smart-tab mode to have:
;; Tab key once will indent like always
;; Tab key twice will try to expand the current 'expression'
(setq hippie-expand-try-functions-list
  '(
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
;;     try-expand-line
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     ))

(provide '07-completion)

;;; 07-completion.el ends here
