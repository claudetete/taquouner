;;; 06-interface-00-main-window.el --- a config file for Emacs interface

;; Copyright (c) 2006-2017 Claude Tete
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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.5
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;; section comment
;; [HEADER.modification of GNU Emacs interface, size, title, decoration...]
;;
;; subsection comment
;; [SUBHEADER.modification about main window of emacs]

;;; Change Log:
;; 2017-07-25 (2.5)
;;    update to new conf format
;; 2016-09-28 (2.4)
;;    add emacs version 25 to use new fullscreen function
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
       (when tqnr-profile-font
         (set-frame-parameter frame 'font tqnr-profile-font))
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
;; [VARIABLE.tqnr-profile-window-title "< %b >"]
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

(provide '06-interface-00-main-window)

;;; 06-interface-00-main-window.el ends here
