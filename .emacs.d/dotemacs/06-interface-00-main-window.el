;;; 06-interface-00-main-window.el --- a config file for Emacs interface

;; Copyright (c) 2006-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
;; Version: 2.6
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;; section comment
;; [HEADER.modification of GNU Emacs interface, size, title, decoration...]
;; [DEFAULT.t]
;;
;; subsection comment
;; [SUBHEADER.modification about main window of emacs]
;; [SUBDEFAULT.t]


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
       (when tqnr-profile-font
         (set-frame-font tqnr-profile-font))
       (frame-maximizer)
       )))

;; [COMMENT.]
;; [VARCOMMENT.DECORATION: remove all mouse interface (toolbar, menubar, scrollbar)]
;; [VARIABLE.tqnr-section-interface-remove-decoration nil]
(when tqnr-section-interface-remove-decoration (message "    Remove Decoration...")
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
  (message "    Remove Decoration... Done"))

;; [COMMENT.]
;; [[VARCOMMENT.WINDOW TITLE: buffer name in title bar (example "< foobar.c >") (from grandm_y)
;; %b buffername ; %F frame name ; %l line number ; %c column number
;; %p percent of buffer above top ; %m mode name ; %n Narrow mode
;; %z coding systems ; %Z %z + end-of-line format ; %- infinitely dashes
;; ]]
;; [VARIABLE.tqnr-profile-window-title "GNU Emacs - %b"]
(if tqnr-profile-window-title
  (setq frame-title-format tqnr-profile-window-title)
  (setq frame-title-format "<[ %b ]>")
  ) ; (if tqnr-profile-window-title

;; [COMMENT.]
;; [VARCOMMENT.TRANSPARENCY: the whole emacs window will be transparent]
;; [VARIABLE.tqnr-section-interface-transparency nil]
(when tqnr-section-interface-transparency (message "      Transparency...")
  (when tqnr-running-in-graphical
    ;; [VARCOMMENT.transparency of the window. 0=transparent/100=opaque]
    ;; [VARIABLE.tqnr-profile-transparency 96]
    (set-frame-parameter (selected-frame) 'alpha tqnr-profile-transparency)
    )
  (message "      Transparency... Done"))

;; [COMMENT.]
;; [VARCOMMENT.FULLSCREEN: main window start in fullscreen]
;; [VARIABLE.tqnr-section-interface-fullscreen t]
(when tqnr-section-interface-fullscreen (message "    FullScreen...")
  ;; start in fullscreen
  ;; need to put fullscreen after font settings !
  (if (or tqnr-running-on-emacs-24-4 tqnr-running-on-emacs-24-5 tqnr-running-on-emacs-25)
    (toggle-frame-maximized)
    (frame-maximizer))
  (message "    FullScreen... Done"))

;; [COMMENT.]
;; [VARCOMMENT.Do not popup any window by splitting vertically only horizontally]
;; [VARIABLE.tqnr-section-interface-popup-window-horizontally nil]
(when tqnr-section-interface-popup-window-horizontally
  (setq split-width-threshold nil))


(provide '06-interface-00-main-window)

;;; 06-interface-00-main-window.el ends here
