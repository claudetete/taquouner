;;; 06-interface-02-ecb.el --- a config file for ecb display setting -*- lexical-binding: t -*-

;; Copyright (c) 2010-2019 Claude Tete
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
;; Version: 1.8
;; Created: October 2010
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.set size, display, refresh and remove opening tips of ECB window]
;; [SUBDEFAULT.nil]


;;; Code:
(when tqnr-section-mode-cedet-ecb

  ;; [VARCOMMENT.ECB ICON FOR TREE: display icon image instead of ascii guides for arborescence tree]
  ;; [VARIABLE.tqnr-section-interface-ecb-ascii-tree nil]
  (when tqnr-section-interface-ecb-ascii-tree (message "      ECB Ascii tree...")
    (custom-set-variables
      ;; ascii tree in "ecb directories"
      '(ecb-tree-buffer-style (quote ascii-guides))
      ;;
      ;; no symbol/image are show for tree only '+' or '-'
      '(ecb-tree-expand-symbol-before nil)
      ;;
      ;; ascii tree in "ecb methods" (functions)
      '(ecb-display-image-icons-for-semantic-tags nil)
      )
    (message "      ECB Ascii tree... Done"))

  (custom-set-variables
    ;; check read only files in "ecb sources"
    '(ecb-sources-perform-read-only-check t)
    ;;
    ;; indent of 2 spaces for the tree in "ecb directories"
    '(ecb-tree-indent 2)
    ;;
    ;; first showed line is the parent folder (if it not visible)
    '(ecb-tree-make-parent-node-sticky t)

    ;; disable tip of the day show at each start up
    '(ecb-tip-of-the-day nil)

    ;; width of ecb window (here 10% of the total width of Emacs)
    '(ecb-windows-width 0.1)
    ;;
    ;; height of compile/grep window
    '(ecb-compile-window-height 0.25)
    ;;
    ;; max height of compile/grep window
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
  ) ; when tqnr-section-mode-cedet-ecb


(provide '06-interface-02-ecb)

;;; 06-interface-02-ecb.el ends here
