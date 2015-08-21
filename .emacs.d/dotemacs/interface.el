;;; interface.el --- a config file for Emacs interface

;; Copyright (c) 2006-2015 Claude Tete
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
;; Version: 2.3
;; Created: October 2006
;; Last-Updated: August 2015

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-interface'

;;; Change Log:
;; 2015-08-21 (2.3)
;;    use new function to maximize window (frame) with emacs 24.5
;; 2013-05-30 (2.2)
;;    remove settings for ediff (put in mode.el)
;; 2013-05-23 (2.1)
;;    add window title from profile
;; 2013-05-07 (2.0)
;;    remove condition about os for maximize
;; 2012-08-01 (1.9)
;;    add condition to have transparency and maximizer
;; 2012-07-09 (1.8)
;;    simplify font setting + generic fullscreen
;; 2012-06-08 (1.7)
;;    change fring size + add ediff settings + add hook after open a new frame
;;    (maximize)
;; 2012-06-05 (1.6)
;;    add transparency from display-buffer.el and use profile value
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

;; shrink the size of fring bar (bar at left and right of each buffer
(set-fringe-mode '(8 . 4))

;; maximize a frame when it is created
(add-hook 'after-make-frame-functions
  '(lambda (f)
     (with-selected-frame f
       (when profile-font
         (set-frame-parameter frame 'font profile-font))
       (frame-maximizer)
       )))


;;
;;; DECORATION
;; REQUIREMENT: var     `section-interface-decoration'
(when section-interface-remove-decoration (message "  6.1 Remove Decoration...")
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
  (message "  6.1 Remove Decoration... Done"))

;;
;;; MODELINE
;; REQUIREMENT: var     `section-interface-modeline'
(when section-interface-modeline (message "  6.2 Modeline...")
  (try-require 'interface-modeline "    ")
  (message "  6.2 Modeline... Done"))

;;
;; buffer name in title bar (example "<[ foobar.c ]>") (from grandm_y)
(if profile-window-title
  (setq frame-title-format profile-window-title)
  (setq frame-title-format "<[ %b ]>")
  ) ; (if profile-window-title

;;
;;;; TRANSPARENCY
;; REQUIREMENT: var     `section-interface-transparency'
;;              var     `section-environment-terminal-vs-graphics'
(when section-interface-transparency (message "    6.3 Transparency...")
  (when running-in-graphical
    (set-frame-parameter (selected-frame) 'alpha profile-transparency)
    )
  (message "    6.3 Transparency... Done"))

;;
;;; FULLSCREEN
;; REQUIREMENT: var     `section-interface-fullscreen'
;;              var     `section-environment-terminal-vs-graphics'
;;              var     `section-environment-os-recognition'
(when section-interface-fullscreen (message "  6.4 FullScreen...")
  ;; start in fullscreen
  ;; need to put fullscreen after font settings !
  (if (or running-on-emacs-24-4 running-on-emacs-24-5)
    (toggle-frame-maximized)
    (frame-maximizer))
  (message "  6.4 FullScreen... Done"))

;;
;;; ECB
(when section-interface-ecb (message "  6.5 Ecb...")
  (try-require 'interface-ecb "    ")
  (message "  6.5 Ecb... Done"))

(provide 'interface)

;;; interface.el ends here
