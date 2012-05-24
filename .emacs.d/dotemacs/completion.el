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
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-completion'

;;; Change Log:
;; 2012-05-10 (1.3)
;;    fix bug with CUA rectangle selection by disable dynamic completion +
;;    remove hippie do not work properly
;; 2012-05-04 (1.2)
;;    add configuration for hippie expand
;; 2012-03-28 (1.1)
;;    translate comments in English
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch



;;; Code:
;; completion is case sensitive
(defvar dabbrev-case-replace nil)

;;;; enable dynamic word completion (never used)
;;;; from grandm_y
;;;; Interfere with CUA selection mode (insert only null character)
;;(dynamic-completion-mode)

;;; completion.el ends here
