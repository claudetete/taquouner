;;; display-font.el --- a config file for font setting

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

;; Keywords: config, display, font
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-font'
;;              var     `section-environment-os-recognition'
;;              var     `section-environment-terminal-vs-graphics'

;;; Change Log:
;; 2012-03-02 (1.3)
;;    add some elegant font for other than simple ascii character
;; 2011-07-09 (1.2)
;;    add color-theme
;; 2011-04-21 (1.1)
;;    add test about ms-window and graphics
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;
;;; MS Windows
(if running-on-ms-windows
  (progn
    (if running-in-graphical
      ;; Graphique
;;; impossible d'utiliser misc-fixed ??
      ;; to have only ascii and very tiny
      ;;(progn (set-face-font 'default "-raster-Terminal-medium-normal-normal-mono-8-*-*-*-c-*-ms-oemlatin"))
      ;; to have accent character but bigger
      (progn (set-face-font 'default "-raster-ProggyTinySZ-normal-normal-normal-mono-10-*-*-*-c-*-iso8859-1"))
      ;; Terminal (Cygwin)
      (progn (set-face-font 'default "-raster-Terminal-normal-normal-normal-mono-8-*-*-*-c-*-ms-oemlatin"))
      )
    )
  )

;;
;;; INTERNATIONAL
;; REQUIREMENT: var     `section-display-font-international'
(when section-display-font-international (message "    6.4.1 International...")
  ;;;; Support accents et UTF-8
  ;;(setq-default enable-multibyte-characters 1)
  ;;(if (not (featurep 'xemacs))
  ;;  (prefer-coding-system 'utf-8)
  ;;;; Au XXIème siècle, Unicode ne marche juste pas sous xemacs
  ;;;; http://www.xemacs.org/Documentation/21.5/html/xemacs-faq_2.html#SEC72
  ;;;; Alors on fait ce qu'on peut.
  ;;(require 'un-define)
  ;;(set-coding-priority-list '(utf-8))
  ;;(set-coding-category-system 'utf-8 'utf-8)
  ;;;; TODO : XEmacs 21.4.21 n'affiche même pas les accents par défaut
  ;;;; lorsqu'il ouvre un fichier Unicode, sauf si on fait :
  ;;(standard-display-european t)
  ;;;; auquel cas... il se plante !!
  ;;  )
  (message "    6.4.1 International... Done"))

;;
;; autres caracteres sont affiches comme ceci : ^@
(setq-default ctl-arrow t)

;;; display-font.el ends here
