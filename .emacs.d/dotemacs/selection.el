;;; selection.el --- a config file for text selection settings

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

;; Keywords: config, selection, text
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-selection'

;;; Change Log:
;; 2012-03-02 (1.2)
;;    add else for selection with shift key
;; 2011-04-21 (1.1)
;;    add section for selection with shift
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch + shift selection + color (no history since)


;;; Code:
;; Possibilite de supprimer la selection
(delete-selection-mode t)
;;
;; permet d'avoir la selection en couleur
(setq transient-mark-mode t)

;;
;;; SHIFT SELECTION
;; REQUIREMENT: var     `section-selection-with-shift'
(if section-selection-with-shift
  (progn
    (message "  5.1 Selection with Shift...")
    ;; selection avec shift
    (setq shift-selection-mode t)
    (message "  5.1 Selection with Shift... Done")
    )
  (setq shift-selection-mode nil)
  )

;; permet de copier meme si en lecture seul
(setq kill-read-only-ok t)

;;; selection.el ends here
