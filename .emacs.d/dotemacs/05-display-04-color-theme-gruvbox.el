;;; 05-display-04-color-theme-gruvbox.el --- configuration of gruvbox theme -*- lexical-binding: t -*-

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
;; [SUBHEADER.Gruvbox theme config]
;; [SUBDEFAULT.nil]


;;; Code:

;; for gruvbox-dark-hard
;;(gruvbox-dark0           "#282828" "#262626")
;;(gruvbox-dark2           "#504945" "#4e4e4e")
;;(gruvbox-light0          "#fdf4c1" "#ffffaf")
;;(gruvbox-light2          "#d5c4a1" "#bcbcbc")
;;(gruvbox-bright_red      "#fb4933" "#d75f5f")
;;(gruvbox-bright_yellow   "#fabd2f" "#ffaf00")
;;(gruvbox-dark0_hard      "#1d2021" "#1c1c1c")

(when tqnr-section-mode-powerline
  (use-package gruvbox-theme
    :custom-face
    (powerline-active0             ((t (:background "#fabd2f" :foreground "#1d2021"))))
    (powerline-highlight-active0   ((t (:background "#fabd2f" :foreground "#fb4933"))))
    (powerline-active1             ((t (:background "#504945" :foreground "#fdf4c1"))))
    (powerline-highlight-active1   ((t (:background "#504945" :foreground "#fabd2f"))))
    (powerline-active2             ((t (:background "#282828" :foreground "#fdf4c1"))))
    (powerline-highlight-active2   ((t (:background "#282828" :foreground "#fabd2f"))))
    (powerline-inactive0           ((t (:background "#d5c4a1" :foreground "#1d2021"))))
    (powerline-highlight-inactive0 ((t (:background "#d5c4a1" :foreground "#1d2021"))))
    (powerline-inactive1           ((t (:background "#504945" :foreground "#d5c4a1"))))
    (powerline-highlight-inactive1 ((t (:background "#504945" :foreground "#d5c4a1"))))
    (powerline-inactive2           ((t (:background "#282828" :foreground "#d5c4a1"))))
    (powerline-highlight-inactive2 ((t (:background "#282828" :foreground "#d5c4a1"))))))

(when tqnr-section-mode-yascroll
  (use-package gruvbox-theme
    :custom-face
    (yascroll:thumb-text-area ((t (:background "#fabd2f"))))
    (yascroll:thumb-fringe    ((t (:foreground "#fabd2f" :background "#fabd2f"))))))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))


(provide '05-display-04-color-theme-gruvbox)

;;; 05-display-04-color-theme-gruvbox.el ends here
