;;; shortcut-grep.el --- a config file for grep shortcut

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

;; Keywords: config, shortcut, grep
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-grep'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-03-30 (1.3)
;;    translate comments in English
;; 2012-03-22 (1.2)
;;    add condition about working environment
;; 2011-07-27 (1.1)
;;    change shortcut for grep-find by project
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
(cond
;; Magneti Marelli -------------------------------------------------------------
  ((string= profile "Magneti Marelli")
    ;; custom grep for NBNF LL project
    (global-set-key     "\C-cnn"                'nll-grep-find)
    ;;
    ;; custom grep for NBNF HL project
    (global-set-key     "\C-cnh"                'nhl-grep-find)
    ;;
    ;; custom grep for NSF project
    (global-set-key     "\C-cns"                'nsf-grep-find)
    ;;
    ;; custom grep for ENSF project
    (global-set-key     "\C-cne"                'ecar-grep-find)
    ) ; Magneti Marelli
  ) ; cond ---------------------------------------------------------------------

;; next occurrence of grep
(global-set-key         [f3]                    'next-match)
;;
;; previous occurrence of grep
(global-set-key         [f5]                    'previous-error)
;;
;; next occurrence
(global-set-key         [f6]                    'occur-next-error)
;;
;; previous occurrence
(global-set-key         [f7]                    'compilation-previous-error)

;; to compile
(global-set-key         [f10]                   'compile)
;;
;;;; next error
;;(global-set-key         [f12]                   'next-error)


(provide 'shortcut-grep)

;;; shortcut-grep.el ends here
