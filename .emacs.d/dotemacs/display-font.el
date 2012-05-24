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
;; Version: 1.5
;; Created: October 2006
;; Last-Updated: April 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-font'
;;              var     `section-environment-os-recognition'
;;              var     `section-environment-terminal-vs-graphics'

;;; Change Log:
;; 2012-04-20 (1.5)
;;    add working environment
;; 2012-03-28 (1.4)
;;    translate comments in English
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

(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    ;; to have only ascii and very very tiny
    (set-face-font 'default "-raster-Terminal-normal-normal-normal-mono-8-*-*-*-c-*-ms-oemlatin")
    ) ; Magneti Marelli

  ;; Alstom Transport ----------------------------------------------------------
  ((string= clt-working-environment "Alstom Transport")
    (if running-in-graphical
      ;; to have accent character and tiny
      (progn (set-face-font 'default "-raster-ProggyTinySZ-normal-normal-normal-mono-10-*-*-*-c-*-iso8859-1"))
      ;; Terminal (Cygwin)
      (progn (set-face-font 'default "-raster-Terminal-normal-normal-normal-mono-8-*-*-*-c-*-ms-oemlatin"))
      )
    ) ; Alstom Transport

    ;; default -----------------------------------------------------------------
    ((string= clt-working-environment "default")
      (if running-on-ms-windows
        (progn
          ;;FIXME working environment default
          ;;; Anonymous Pro, 10
          ;; nice, big (slashed 'zero', 'one' and minus 'L' can be mixed up)
          ;(set-face-font 'default "-outline-Anonymous Pro-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")

          ;;; Anonymous Pro, 8
          ;; nice, small (slashed 'zero', 'one' and minus 'L' can be mixed up, parentheses and curly bracket can be mixed up)
          ;(set-face-font 'default "-outline-Anonymous Pro-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1")

          ;;; Proggy Tiny Z, 8
          ;; good, very tiny (slashed 'zero', dot and comma can be mixed)
          ;(set-face-font 'default "-raster-ProggyTinySZ-normal-normal-normal-mono-10-*-*-*-c-*-iso8859-1")

          ;;; DejaVu Sans Mono, 10
          ;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
          ;(set-face-font 'default "-outline-DejaVu Sans Mono-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")

          ;;; Inconsolata, 9
          ;; not so good with ms window (slashed 'zero', capitalized 'i' and minus 'L' can be mixed up)
          ;(set-face-font 'default "-outline-Inconsolata-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")

          ;;; Lucida Console, 10
          ;; nice, big, large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
          ;(set-face-font 'default "-outline-Lucida Console-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")

          ;;; Lucida Console, 8
          ;; nice, small large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
          ;(set-face-font 'default "-outline-Lucida Console-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1")

          ;;; Monaco, 10
          ;; nice, very big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
          ;(set-face-font 'default "-outline-Monaco-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")

          ;; Monaco, 8
          ;; nice, big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
          ;(set-face-font 'default "-outline-Monaco-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1")

          ;; ProFont, 8
          ;; nice, tiny, (slashed 'zero', 'one' and minus 'L' can be mixed up)
          ;(set-face-font 'default "-outline-ProFontWindows-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1")
          )
        (progn
          (set-face-font 'default "to be defined")
          )
        )
      ) ; default

  ) ; cond -------------------------------------------------------------------

;;
;;; INTERNATIONAL
;; REQUIREMENT: var     `section-display-font-international'
(when section-display-font-international (message "    5.3.1 International...")
  ;; no more setting to have Unicode
  (message "    5.3.1 International... Done"))

;;
;; all other characters will be displayed like this: ^@
(setq-default ctl-arrow t)

;;; display-font.el ends here
