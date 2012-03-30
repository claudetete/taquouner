;;; display-ecb.el --- a config file for ecb display setting

;; Copyright (c) 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, display, ecb
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.4
;; Created: October 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-ecb'
;;              var     `section-mode-cedet-ecb'

;;; Change Log:
;; 2012-03-28 (1.4)
;;    translate comments in english
;; 2012-03-12 (1.3)
;;    add condition about ecb active or not
;; 2012-07-09 (1.1)
;;    add max size of compil window
;; 2011-04-21 (1.1)
;;    add delay before refresh
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2010-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
(when section-mode-cedet-ecb
  (custom-set-variables
    ;; check read only files in "ecb sources"
    '(ecb-sources-perform-read-only-check t)

    ;; ascii tree in "ecb directories"
    '(ecb-tree-buffer-style (quote ascii-guides))
    ;;
    ;; no symbol/image are show for tree only '+' or '-'
    '(ecb-tree-expand-symbol-before nil)
    ;;
    ;; indent of 2 spaces for the tree in "ecb directories"
    '(ecb-tree-indent 2)
    ;;
    ;; first showed line is the parent folder (if it not visible)
    '(ecb-tree-make-parent-node-sticky t)
    ;;
    ;; ascii tree in "ecb methods" (functions)
    '(ecb-display-image-icons-for-semantic-tags nil)

    ;; disable tip of the day show at each start up
    '(ecb-tip-of-the-day nil)

    ;;;; width of ecb window (here 10% of the total width of Emacs)
    ;;'(ecb-windows-width 0.1) ;see mystart-up in functions.el
    ;;
    ;;;; height of compil/grep window
    ;;'(ecb-compile-window-height 25) ;see mystart-up in functions.el
    ;;
    ;; max height of compil/grep window
    ;;  - value > 1 -> size in character
    ;;  - 0.0 < value < 1.0 -> size in percent of height
    '(ecb-enlarged-compilation-window-max-height 0.25)

    ;; tags apropos display more data
    '(tags-apropos-verbose t)

    ;; define the buffer name of analyze
    '(ecb-analyse-buffer-name "*ECB Analyse*")

    ;; highlight function in methods window
    '(ecb-highlight-tag-with-point (quote highlight-scroll))

    ;; increase delay before refresh of highlight function to avoid slow down
    ;; browsing
    '(ecb-highlight-tag-with-point-delay 0.5)
    )
  ) ; when section-mode-cedet-ecb

;;; display-ecb.el ends here
