;;; 09-mouse.el --- a config file for mouse settings

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
;; Version: 2.0
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [HEADER.smooth wheel + lazy decoration when scroll]
;; [DEFAULT.t]


;;; Code:
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)


;; Setting this to t makes scrolling faster, but may momentarily present
;; unfontified areas when you scroll into them.
(setq lazy-lock-defer-on-scrolling t)

;;
;; [SUBCOMMENT.PASTE CURSOR: yank at point and not at mouse cursor (either when yank with mouse wheel)]
;; [SUBSECTION.tqnr-section-mouse-paste-to-point-not-mouse-cursor nil]
(when tqnr-section-mouse-paste-to-point-not-mouse-cursor (message "    Paste to point...")
  ;; yank at point and not mouse cursor
  (setq mouse-yank-at-point t)
  (message "    Paste to point... Done"))

;;
;; [SUBCOMMENT.AVOIDANCE: move mouse cursor at top right of the buffer to not bother me]
;; [SUBSECTION.tqnr-section-mouse-avoidance nil]
(when tqnr-section-mouse-avoidance (message "    Mouse Avoidance...")
  ;; move mouse cursor at top right of the buffer to not bother me
  (when (and tqnr-section-environment-terminal-vs-graphics tqnr-running-in-graphical)
    (when (and (display-mouse-p) (require 'avoid nil t))
      (mouse-avoidance-mode 'banish)
      )
    )
  (message "    Mouse Avoidance... Done"))

;;
;; [SUBCOMMENT.SMOOTH SCROLL: scroll with margin and without jump]
;; [SUBSECTION.tqnr-section-mouse-smooth-scroll t]
(when tqnr-section-mouse-smooth-scroll (message "    Smooth Scroll...")
  ;; to avoid jump from (http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling)
  ;; set with max value
  (setq scroll-conservatively most-positive-fixnum)
  ;; do nothing ?
  ;;(setq auto-window-vscroll nil)

  ;; do not work under ms window and small font (100 lines by window)
  ;;(when (try-require 'smooth-scrolling)
  ;;  ;; set margin (where cursor cannot go)
  ;;  (setq smooth-scroll-margin 2))
  (message "    Smooth Scroll... Done"))


(provide '09-mouse)

;;; 09-mouse.el ends here
