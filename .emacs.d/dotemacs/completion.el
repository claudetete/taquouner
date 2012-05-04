;;; completion.el --- a config file for completion settings

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;; Keywords: config, completion
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.2
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-completion'

;;; Change Log:
;; 2012-05-04 (1.2)
;;    add configuration for hippie expand
;; 2012-03-28 (1.1)
;;    translate comments in english
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch



;;; Code:
;; completion is case sensitive
(defvar dabbrev-case-replace nil)

;; enable dynamic word completion
;; from grandm_y
(dynamic-completion-mode)

;; HIPPIE
;; expand text trying various ways to find its expansion (by Fabrice Niessen)
(when (try-require 'hippie-exp)
  ;; list of expansion functions tried (in order) by `hippie-expand'
  (setq hippie-expand-try-functions-list
    '(
       try-expand-dabbrev   ; from current buffer
       try-expand-dabbrev-visible   ; from visible parts of all windows
       try-expand-dabbrev-all-buffers   ; from all other buffers
       try-expand-dabbrev-from-kill
       try-complete-file-name-partially
       try-complete-file-name
       try-expand-all-abbrevs
       try-expand-list
       try-expand-line
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol
       try-expand-whole-kill
       )
    )
)

;;; completion.el ends here
