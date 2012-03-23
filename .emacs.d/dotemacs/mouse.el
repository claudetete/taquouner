;;; mouse.el --- a config file for mouse settings

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

;; Keywords: config, mouse
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: October 2006
;; Last-Updated: February 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-mouse'
;;              car     `section-environment-os-recognition'

;;; Change Log:
;; 2012-02-28 (1.2)
;;    add lazy fontify when scroll
;; 2011-04-21 (1.1)
;;    add test about graphics
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; active la smooth roulette (stealing from y grandmaitre)
(setq scroll-step 1)

;;
;;; PASTE CURSOR
;; REQUIREMENT: var     `section-mouse-paste-to-point-not-mouse-cursor'
(when section-mouse-paste-to-point-not-mouse-cursor (message "  9.1 Paste to point...")
  ;; coller au curseur pas a la souris
  (setq mouse-yank-at-point t)
  (message "  9.1 Paste to point... Done"))

;; bouger le curseur au bord de l'ecran lors de la saisie
(when running-in-graphical
  (when (and (display-mouse-p) (require 'avoid nil t))
    ;; Move the mouse to the lower-right corner instead of default up-right
    (mouse-avoidance-mode 'banish)
    )
  )

;; Setting this to t makes scrolling faster, but may momentarily present
;; unfontified areas when you scroll into them.
(setq lazy-lock-defer-on-scrolling t)
