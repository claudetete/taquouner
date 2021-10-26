;;; 05-display-05-color-theme-sanityinc-tomorrow.el --- configuration of sanityinc-tomorrow theme -*- lexical-binding: t -*-

;; Copyright (c) 2021 Claude Tete
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
;; Version: 0.1
;; Created: January 2021
;; Last-Updated: January 2021

;;; Commentary:
;;
;; [SUBHEADER.Sanityinc Tomorrow theme config]
;; [SUBDEFAULT.nil]


;;; Code:

(use-package color-theme-sanityinc-tomorrow
  :config
  (add-to-list 'color-theme-sanityinc-tomorrow-colors
    '(dark . (
               (background     . "#1d1d1d")
               (alt-background . "#22a222a222a2")
               (current-line   . "#282828")
               (selection      . "#323232")
               (foreground     . "#cccccc")
               (comment        . "#969696")
               (red            . "#f2777a")
               (orange         . "#f99157")
               (yellow         . "#ffcc66")
               (green          . "#99cc99")
               (aqua           . "#66cccc")
               (blue           . "#6699cc")
               (purple         . "#cc99cc"))))

  (defun color-theme-sanityinc-tomorrow-dark ()
    "Apply the tomorrow dark theme."
    (interactive)
    (color-theme-sanityinc-tomorrow 'dark))

  (add-to-list 'custom-theme-load-path 
    (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes"))
  (load-theme 'sanityinc-tomorrow-dark t)

  :custom-face
  (ivy-cursor ((t (:background "#f2777a"))))
  )

(when tqnr-section-mode-powerline
  (use-package color-theme-sanityinc-tomorrow
    :custom-face
    (powerline-active0             ((t (:foreground "#323232" :background "#99cc99"))))
    (powerline-inactive0           ((t (:foreground "#323232" :background "#969696"))))
    (powerline-highlight-active0   ((t (:foreground "#f2777a" :background "#99cc99"))))
    (powerline-highlight-inactive0 ((t (:foreground "#f2777a" :background "#969696"))))
    (powerline-active1             ((t (:foreground "#cccccc" :background "#282828"))))
    (powerline-inactive1           ((t (:foreground "#cccccc" :background "#282828"))))
    (powerline-highlight-active1   ((t (:foreground "#f99157" :background "#282828"))))
    (powerline-highlight-inactive1 ((t (:foreground "#cccccc" :background "#282828"))))
    (powerline-active2             ((t (:foreground "#cccccc" :background "#22a222a222a2"))))
    (powerline-inactive2           ((t (:foreground "#cccccc" :background "#22a222a222a2"))))
    (powerline-highlight-active2   ((t (:foreground "#f99157" :background "#22a222a222a2"))))
    (powerline-highlight-inactive2 ((t (:foreground "#cccccc" :background "#22a222a222a2"))))))

(when tqnr-section-mode-yascroll
  (use-package color-theme-sanityinc-tomorrow
    :custom-face
    (yascroll:thumb-text-area ((t (:background "#cccccc"))))
    (yascroll:thumb-fringe    ((t (:foreground "#cccccc" :background "#cccccc"))))))


(provide '05-display-05-color-theme-sanityinc-tomorrow)

;;; 05-display-05-color-theme-sanityinc-tomorrow.el ends here
