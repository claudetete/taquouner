;;; shortcut-buffers.el --- a config file for buffer shortcut

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

;; Keywords: config, shorcut, buffer
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-buffers'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-03-23 (1.1)
;;    change kill this buffer shortcut to have logical ecb shortcut
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; ferme le buffer courant
(global-set-key         (kbd "M-`")            'kill-this-buffer)
;;
;; f11 pour switcher entre les buffer puis enter
(global-set-key         [f11]                   'switch-to-buffer)
;;
;; affiche une liste des buffers selectionnable  /* deprecated */
(global-set-key         (kbd "C-x C-b")         'electric-buffer-list)
;;
;; ouvrir fenetre des bookmarks
(global-set-key         "\C-cb"                 'bookmark-bmenu-list)

;;;; va au buffer suivant /* suit la logique des editeurs classiques */
;;(global-set-key         [(control tab)]         'previous-user-buffer)
;;;;
;;;; va au buffer precedent /* suit la logique des editeurs classiques */
;;(global-set-key         [(control backtab)]     'next-user-buffer)
;;
;; va au buffer suivant /* suit la logique des editeurs classiques */
(global-set-key         [(control x) (right)]   'previous-user-buffer)
;;
;; va au buffer precedent /* suit la logique des editeurs classiques */
(global-set-key         [(control x) (left)]    'next-user-buffer)

;;; shortcut-buffers.el ends here
