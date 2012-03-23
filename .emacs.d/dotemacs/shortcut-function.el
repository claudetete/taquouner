;;; shortcut-function.el --- a config file for function shortcut

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

;; Keywords: config, shorcut, function
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.0
;; Created: October 2006
;; Last-Updated: March 2011

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-function'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-03-19 (1.3)
;;    add align shortcut and replace for rtrt
;; 2011-11-03 (1.2)
;;    add preprocessing funtion for each project
;; 2011-10-27 (1.1)
;;    comment the Magneti CAN Changelog insert
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;;; inserer truc dans can changelog.txt
;;(global-set-key "\C-cr"                 'mm-can-insert-del-signal)
;;(global-set-key "\C-cf"                 'mm-can-insert-add-signal)
;;(global-set-key "\C-ct"                 'mm-can-insert-del-message)
;;(global-set-key "\C-cg"                 'mm-can-insert-add-message)
;;(global-set-key "\C-cd"                 'mm-can-insert-separation)

;; selectionne le mot entier sous le curseur
(global-set-key "\C-\M-z"               'select-word-under)
(global-set-key "\C-\M-c"           'occur-word-at-point)

;; lance les modes au demarrage
(global-set-key         "\C-cl"             'mystart-up)

;; use align regexp for .ptu file (rtrt script)
(global-set-key         "\C-cpo"            'rtrt-align-init)
(global-set-key         "\C-cp;"            'rtrt-align-ev)
(global-set-key         "\C-cp["            'rtrt-align-declaration)

;; format the .ptu file (rtrt script)
(global-set-key         "\C-crv"            'rtrt-upcase-var-string)
(global-set-key         "\C-crs"            'rtrt-remove-whitespace-before-colon)

;;;; preprocess entierement une macro pour le projet NSF
;;(global-set-key         "\C-cms"            'nsf-c-expand-macro)
;;;; preprocess entierement une macro pour le projet NBNF_HL
;;(global-set-key         "\C-cmh"            'nhl-c-expand-macro)
;;;; preprocess entierement une macro pour le projet NBNF_LL
;;(global-set-key         "\C-cmn"            'nll-c-expand-macro)
;;;; preprocess entierement une macro pour le projet ECAR
;;(global-set-key         "\C-cme"            'ecar-c-expand-macro)
;;;; preprocess entierement une macro pour le projet ECAR
;;(global-set-key         "\C-cmx"            'xl1-c-expand-macro)

;;; shortcut-function.el ends here
