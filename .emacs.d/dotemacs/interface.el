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
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-interface'

;;; Change Log:
;; 2012-03-29 (1.3)
;;    translate comments in english
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
;; show shortcut of last command launch with M-x in minibuffer
(setq teach-extended-commands-p t)

;;
;;; DECORATION
;; REQUIREMENT: var     `section-interface-decoration'
(when section-interface-decoration (message "  1.1 Decoration...")
  ;; no more menu bar (to open it Ctrl + right click)
  (and (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
  ;;
  ;; no more scroll bar
  (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;;
  ;; no more toolbar
  (and (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
  ;;
  ;; no more tooltip (I don't know if this works)
  (and (fboundp 'tooltip-mode) (fboundp 'x-show-tip) (tooltip-mode -1))
  (message "  1.1 Decoration... Done"))

;;
;;; FULLSCREEN
;; REQUIREMENT: var     `section-interface-fullscreen'
;;              var     `section-environment-os-recognition'
(when section-interface-fullscreen (message "  1.2 FullScreen...")
  ;; start in fullscreen (only MS Windows and does not work with vanilla)
  (when running-on-ms-windows
    (add-hook 'term-setup-hook
      #'(lambda () (w32-send-sys-command ?\xF030)))
    )
  (message "  1.2 FullScreen... Done"))

;;
;;; MODELINE
;; REQUIREMENT: var     `section-interface-modeline'
(when section-interface-modeline (message "  1.3 Modeline...")
  ;; show column number in modeline
  (column-number-mode t)

  ;; show time in 24H format (example 23:59)
  (setq display-time-24hr-format t)
  ;;
  ;; show time and date in modeline
  (setq display-time-day-and-date t)

  ;; show date in european format (example: jeu. 29 mars)
  (custom-set-variables
    '(display-time-string-forms
       (quote
         ((if
            (and
              (not display-time-format)
              display-time-day-and-date)
            (format-time-string "%a %e %b " now) "")
           (propertize
             (format-time-string
               (or display-time-format
                 (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
               now)
             (quote help-echo)
             (format-time-string "%a %e %b, %Y" now))
           load
           (if mail
             (concat " "
               (propertize display-time-mail-string (quote display)
                 (\` (when
                       (and display-time-use-mail-icon
                         (display-graphic-p))
                       (\,@ display-time-mail-icon)
                       (\,@ (if
                              (and display-time-mail-face
                                (memq
                                  (plist-get
                                    (cdr display-time-mail-icon)
                                    :type)
                                  (quote (pbm xbm))))
                              (let
                                ((bg
                                   (face-attribute display-time-mail-face
                                     :background)))
                                (if
                                  (stringp bg)
                                  (list :background bg)))))))
                 (quote face) display-time-mail-face (quote help-echo) "You have new mail; mouse-2: Read mail" (quote mouse-face) (quote mode-line-highlight) (quote local-map)
                 (make-mode-line-mouse-map (quote mouse-2) read-mail-command)))
             ""))))
    )
  ;;
  ;; show time in modeline
  (display-time-mode t)

  ;; display size of file in the modeline
  (size-indication-mode t)
  (message "  1.3 Modeline... Done"))

;;
;; buffer name in title bar (example "<[ foobar.c ]>") (from grandm_y)
(setq frame-title-format "<[ %b ]>")

;;; interface.el ends here
