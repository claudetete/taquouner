;;; interface.el --- a config file for Emacs interface

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

;; Keywords: config, interface
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.5
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-interface'

;;; Change Log:
;; 2012-05-03 (1.5)
;;    fix bug about fullscreen and ecb
;; 2012-04-25 (1.4)
;;    replace setting to remove tooltip
;; 2012-03-29 (1.3)
;;    translate comments in English
;; 2012-03-02 (1.2)
;;    remove "display" in name file et section
;; 2011-04-21 (1.1)
;;    add test about ms-windows to set fullscreen
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; sort the buffer menu (open with control + left click)
(msb-mode 1)
;;
;; show shortcut of last command launch with M-x in Minibuffer
(setq teach-extended-commands-p t)

;;
;;; DECORATION
;; REQUIREMENT: var     `section-interface-decoration'
(when section-interface-decoration (message "  6.1 Decoration...")
  ;; no more menu bar (to open it Ctrl + right click)
  (and (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
  ;;
  ;; no more scroll bar
  (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;;
  ;; no more toolbar
  (and (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
  ;;
  ;; no more tooltips (delay of 9999 seconds before displayed)
  (setq tooltip-delay 9999)
  (message "  6.1 Decoration... Done"))

;;
;;; MODELINE
;; REQUIREMENT: var     `section-interface-modeline'
(when section-interface-modeline (message "  6.2 Modeline...")
  (load-file (concat dotemacs-path "/dotemacs/interface-modeline.el"))
  (message "  6.2 Modeline... Done"))

;;
;; buffer name in title bar (example "<[ foobar.c ]>") (from grandm_y)
(setq frame-title-format "<[ %b ]>")

;;
;;; FULLSCREEN
;; REQUIREMENT: var     `section-interface-fullscreen'
;;              var     `section-environment-os-recognition'
(when section-interface-fullscreen (message "  6.3 FullScreen...")
  ;; start in fullscreen (only MS Windows)
  ;; need to put fullscreen after font settings
  (when running-on-ms-windows
    ;; send command FullScreen to the window of Emacs
    (w32-send-sys-command 61488)

    ;; annoying but needed to wait true fullscreen
    ;; wait 1 second
    (sit-for 1)

    )
  (message "  6.3 FullScreen... Done"))

;;; interface.el ends here
