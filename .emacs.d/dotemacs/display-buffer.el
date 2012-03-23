;;; display-buffer.el --- a config file for window/buffer display setting

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

;; Keywords: config, display, buffer, window
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-windows-buffers'

;;; Change Log:
;; 2012-03-20 (1.1)
;;    add buffer name to list of compilation window
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; Define buffers that should appear in the same window.
(add-to-list 'same-window-buffer-names "*Apropos*")
(add-to-list 'same-window-buffer-names "*ccm*")
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "*Colors*")
(add-to-list 'same-window-buffer-names "*Command History*")
(add-to-list 'same-window-buffer-names "*Diff*")
(add-to-list 'same-window-buffer-names "*Proced*")
(add-to-list 'same-window-buffer-names "*vc-dir*")
(add-to-list 'same-window-buffer-names "msg.txt")
(add-to-list 'same-window-buffer-names "*GTAGS SELECT*")
(add-to-list 'same-window-buffer-names "*Completions*")
(add-to-list 'same-window-regexps "\\*GTAGS SELECT\\* ([A-Z])\\sw*")
(add-to-list 'same-window-regexps "\\*compilation\\*\\(\\|<[0-9]+>\\)")
(add-to-list 'same-window-regexps "\\*grep\\*\\(\\|<[0-9]+>\\)")
(add-to-list 'same-window-regexps "\\*Help\\*\\(\\|<[0-9]+>\\)")
(add-to-list 'same-window-regexps "\\*Symref .*")

;; affiche la limite du buffer sur la gauche
;; represente avec fleches et petit L pour la EOF
(setq-default indicate-buffer-boundaries (quote left))

;;
;;;; TRANSPARENCY
;; REQUIREMENT: var     `section-display-windows-buffers-transparency'
(when section-display-windows-buffers-transparency (message "    6.1.1 Transparency...")
  ;; permet de rendre transparent tout emacs
  (set-frame-parameter (selected-frame) 'alpha '(90 90))
  (add-to-list 'default-frame-alist '(alpha 90 90))
  (message "    6.1.1 Transparency... Done"))


;; affiche 5 ligne en commun lors d'une page suivante/precedente
(custom-set-variables
  '(next-screen-context-lines 5))

;;; do not work properly
;;; (custom-set-faces
;;;   '(default ((t (:stipple "d:/wallpaper.png")))))

;;; display-buffer.el ends here
