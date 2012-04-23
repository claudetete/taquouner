;;; shortcut-windows.el --- a config file for windows shortcut

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

;; Keywords: config, shorcut, window
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.2
;; Created: October 2006
;; Last-Updated: April 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-windows'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-04-17 (1.2)
;;    remove M-s and M-arrow to switch between window
;; 2012-03-30 (1.1)
;;    split .emacs file
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;;; switch between window (like 'C-x o' but can be easily repeat
;;;; use S-arrow to navigate between windows
;;(global-set-key         "\M-s"                  'other-window)

;; use default binding to move between window
;; S-arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; resize window more easily (before `C-x {'...)
;; verical
(global-set-key         (kbd "<C-S-up>")        'enlarge-window)
(global-set-key         (kbd "<C-S-down>")      'shrink-window)
;; horizontal
(global-set-key         (kbd "<C-S-left>")      'enlarge-window-horizontally)
(global-set-key         (kbd "<C-S-right>")     'shrink-window-horizontally)

;;; shortcut-windows.el ends here