;;; my-mouse.el --- a config file for mouse settings

;; Copyright (c) 2006-2013 Claude Tete
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

;; Keywords: config, mouse
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: September 2013

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-mouse'
;;              car     `section-environment-terminal-vs-graphics'

;;; Change Log:
;; 2013-09-10 (1.9)
;;    change options for scrolling
;; 2013-05-23 (1.8)
;;    add option from profile for smooth scroll (with arrows)
;; 2013-05-07 (1.7)
;;    change smooth scroll (to avoid all jump with cursor)
;; 2012-07-09 (1.6)
;;    robustness
;; 2012-06-08 (1.5)
;;    mouse wheel scrolls window under
;; 2012-04-23 (1.4)
;;    add section for mouse avoidance
;; 2012-02-30 (1.3)
;;    translate comments in English
;; 2012-02-28 (1.2)
;;    add lazy fontify when scroll
;; 2011-04-21 (1.1)
;;    add test about graphics
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)


;; Setting this to t makes scrolling faster, but may momentarily present
;; unfontified areas when you scroll into them.
(setq lazy-lock-defer-on-scrolling t)

;;
;;; PASTE CURSOR
;; REQUIREMENT: var     `section-mouse-paste-to-point-not-mouse-cursor'
(when section-mouse-paste-to-point-not-mouse-cursor (message "  9.1 Paste to point...")
  ;; yank at point and not mouse cursor
  (setq mouse-yank-at-point t)
  (message "  9.1 Paste to point... Done"))

;;
;;; AVOIDANCE
;; REQUIREMENT: var     `section-mouse-avoidance'
;;              var     `section-environment-terminal-vs-graphics'
(when section-mouse-avoidance (message "  9.2 Mouse Avoidance...")
  ;; move mouse cursor at top right of the buffer to not bother me
  (when (and section-environment-terminal-vs-graphics running-in-graphical)
    (when (and (display-mouse-p) (require 'avoid nil t))
      (mouse-avoidance-mode 'banish)
      )
    )
  (message "  9.2 Mouse Avoidance... Done"))

;;
;;; SMOOTH SCROLL
;; scroll with margin and without jump
(when section-mouse-smooth-scroll (message "  9.3 Smooth Scroll...")
  ;; to avoid jump from (http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling)
  ;; set with max value
  (setq scroll-conservatively most-positive-fixnum)
  ;; do nothing ?
  ;;(setq auto-window-vscroll nil)

  ;; do not work under ms window and small font (100 lines by window)
  ;;(when (try-require 'smooth-scrolling)
  ;;  ;; set margin (where cursor cannot go)
  ;;  (setq smooth-scroll-margin 2))
  (message "  9.3 Smooth Scroll... Done"))


(provide 'my-mouse)

;;; my-mouse.el ends here
