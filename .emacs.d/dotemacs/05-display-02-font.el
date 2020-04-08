;;; 05-display-02-font.el --- a config file for font setting -*- lexical-binding: t -*-

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
;; Version: 2.3
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.set font in terminal or in graphical]
;; [SUBDEFAULT.t]


;;; Code:
;; [[VARCOMMENT.Font family and size:
;; choice between (it's just some nice font, you can use another font):
;;
;; "Terminal-6"
;; nice, very tiny, only ascii (too tiny ?)
;;
;; "Anonymous Pro-10"
;; nice, big (slashed 'zero', 'one' and minus 'L' can be mixed up)
;; "Anonymous Pro-8"
;; nice, small (slashed 'zero', 'one' and minus 'L' can be mixed up, parentheses and curly bracket can be mixed up)
;;
;; "ProggyTinySZ-6"
;; good, very tiny (slashed 'zero', dot and comma can be mixed)
;;
;; "DejaVu Sans Mono-10"
;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
;; "DejaVu Sans Mono-8"
;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
;;
;; "Inconsolata-10"
;; not so good with ms window (slashed 'zero', capitalized 'i' and minus 'L' can be mixed up)
;;
;; "Lucida Console-10"
;; nice, big, large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
;; "Lucida Console-8"
;; nice, small large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
;;
;; "Monaco-10"
;; nice, very big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
;; "Monaco-8"
;; nice, big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
;;
;; "ProFontWindows-8"
;; nice, tiny, (slashed 'zero', 'one' and minus 'L' can be mixed up)
;;
;; "Courier New-10"
;; classic but big and large
;; "Courier New-8"
;; classic but big and large
;;
;; "Ubuntu Mono-10"
;; "Ubuntu Mono-8"
;;
;; "Terminus-10"
;; "Terminus-8"
;;
;; "Monospace-8"
;;
;; "Liberation Mono-8"
;;
;; "Inconsolata-10"
;; "Inconsolata-8"
;;
;; "Droid Sans Mono-10"
;; "Droid Sans Mono-8"
;;
;; "Iosevka-10"
;; "Iosevka-8"
;; nice can use any variant of this open source font (by example slashed zero, dot zero, empty zero)
;; see https://be5invis.github.io/Iosevka/ for more settings
;;
;; ]]
;; [VARIABLE.tqnr-profile-font "Lucida Console-10"]
(when tqnr-profile-font
  ;; [VARCOMMENT.ANTIALIAS: set antialiasing on font rendering]
  ;; [VARIABLE.tqnr-section-display-font-antialias t]
  (if tqnr-section-display-font-antialias
    (progn (message "      Font Antialias...")
      (setq tqnr-profile-font (concat tqnr-profile-font ":antialias=subpixel"))
      (message "      Font Antialias...Done"))
    (progn
      (setq tqnr-profile-font (concat tqnr-profile-font ":antialias=none"))
      ))

  (if (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
    ;; new function to set font since 23.1
    (set-frame-font tqnr-profile-font)
    ;; obsolete function to set font
    (set-default-font tqnr-profile-font))
  )

;; all other characters will be displayed like this: ^@
(setq-default ctl-arrow t)


(provide '05-display-02-font)

;;; 05-display-02-font.el ends here
