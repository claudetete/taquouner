;;; display-font.el --- a config file for font setting

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

;; Keywords: config, display, font
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.0
;; Created: October 2006
;; Last-Updated: August 2015

;;; Commentary:
;;
;; load by `dotemacs/display.el'
;; REQUIREMENT: var     `section-display-font'

;;; Change Log:
;; 2015-08-21 (2.0)
;;    do not use obsolete function to set font and size
;; 2013-05-07 (1.9)
;;    add antialias option for font
;; 2012-06-26 (1.8)
;;    simplify settings
;; 2012-06-18 (1.7)
;;    add condition about font
;; 2012-06-05 (1.6)
;;    remove all profile dependances = simplify the config
;;    the font is chosen in profile files
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
(when profile-font
  (if section-display-font-antialias
    (progn (message "    5.3.1 Font Antialias...")
      (setq profile-font (concat profile-font ":antialias=subpixel"))
      (message "    5.3.1 Font Antialias...Done"))
    (progn
      (setq profile-font (concat profile-font ":antialias=none"))
      ))
  (if (and section-environment-version-recognition running-on-emacs-24)
    ;; new function to set font since 23.1
    (set-frame-font profile-font)
    ;; obsolete function to set font
    (set-default-font profile-font))
  )

;;
;;; INTERNATIONAL
;; REQUIREMENT: var     `section-display-font-international'
(when section-display-font-international (message "    5.3.2 International...")
  ;; no more setting to have Unicode
  (message "    5.3.2 International... Done"))

;;
;; all other characters will be displayed like this: ^@
(setq-default ctl-arrow t)


(provide 'display-font)

;;; display-font.el ends here
