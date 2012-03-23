;;; annoyances.el --- a config file for all annoying things

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

;; Keywords: config, annoying
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-annoyances'

;;; Change Log:
;; 2012-03-21 (1.3)
;;    add directory for backup file
;; 2012-03-02 (1.2)
;;    add confirm kill + scroll preserve + backup file
;; 2011-07-07 (1.1)
;;    remove shortcut Control+Middle mouse button
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; virer le message d'acceuil
(setq inhibit-startup-message t)

;; C'est penible de taper yes pour confirmer
;; maintenant 'y' pour 'yes' et 'n' pour 'no'
(fset 'yes-or-no-p 'y-or-n-p)
;;
;; ne pas demander pour rafraichir un buffer
(setq revert-without-query '(".*"))

;;
;; virer le insert et le remplacer par suppr (ne marche pas tout le temps)
(global-set-key [insert] 'ignore)
;;
;; virer raccourci C-PageUp & C-PageDown
(global-set-key (kbd "<C-next>") 'ignore)
(global-set-key (kbd "<C-prior>") 'ignore)

;; virer menu to pop for a C-MiddleButton
(global-set-key (kbd "<C-down-mouse-2>") 'ignore)
;;
;; virer les boites de dialogue pour poser des questions
(setq use-dialog-box nil)
;;
;; virer les boites de dialogue pour des demandes sur les fichiers
(setq use-file-dialog nil)
;;
;; ne plus avoir les fichier #foo.bar#
;;; I don't want it
;;;(auto-save-mode nil)
;;
;; ne plus avoir ses saloperies de "tooltips"
(setq tooltip-delay 9999)

;; demande obligatoirement a chaque fermeture de Emacs
;;; do not use it's a pain
;;;(setq confirm-kill-emacs t)

;;
;;;; TRUNCATE LINE
;; REQUIREMENT: var     `section-annoyances-truncate-line'
(when section-annoyances-truncate-line (message "  10.1 Truncate Line...")
  ;; pour ceux qui ne veulent pas avoir les retours a la ligne
  ;; 1 ligne n'est visible que sur une ligne mais pas entierment visible
  (setq-default truncate-lines nil)
  (message "  10.1 Truncate Line... Done"))

;;
;;;; SCROLL PRESERVE CURSOR POSITION
;; REQUIREMENT: var     `section-annoyances-scroll-preserve-cursor-position'
(when section-annoyances-scroll-preserve-cursor-position (message "  10.2 Scroll preserve cursor position...")
  ;; return to same line on a scroll back
  (setq scroll-preserve-screen-position t)
  (message "  10.2 Scroll preserve cursor position... Done"))

;;
;;;; NO BACKUP FILE
;; REQUIREMENT: var     `section-annoyances-no-backup-file'
(when section-annoyances-no-backup-file (message "  10.3 No backup file...")
  ;; Turn off backup files.
  (setq make-backup-files nil)
  (message "  10.3 No backup file... Done"))

;;
;;;; ALL BACKUP FILE IN DIRECTORY
;; REQUIREMENT: var     `section-annoyances-backup-file-in-directory'
(when section-annoyances-backup-file-in-directory (message "  10.4 All backup files in a directory...")
  ;; All backup files goes in a directory.
  (setq backup-directory-alist `(("." . "d:/Users/ctete/tmp/emacs")))
  (setq backup-by-copying t   ; don't clobber symlinks
    version-control t         ; use versioned backups
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2)
  (message "  10.4 All backup files in a directory... Done"))

;;; annoyances.el ends here
