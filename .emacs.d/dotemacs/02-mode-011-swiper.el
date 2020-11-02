;;; 02-mode-011-swiper.el --- configuration of Swiper mode -*- lexical-binding: t -*-

;; Copyright (c) 2020 Claude Tete
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
;; Created: May 2020
;; Last-Updated: May 2020

;;; Commentary:
;;
;; [SUBHEADER.Isearch alternative with fuzzy using ivy]
;; [SUBDEFAULT.nil]


;;; Code:

(use-package swiper
  :bind
  ;; override built-in isearch and isearch-backward
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward)
  ("C-M-v" . swiper-isearch-thing-at-point)

  (:map swiper-isearch-map
    ;; backward way of swiper-C-s to move with C-r
    ("C-r" . swiper-C-r)
    ("M-S-s" . swiper-isearch-toggle)

    ("<M-up>" . ivy-previous-history-element)
    ("<M-down>" . ivy-next-history-element)

    ;; can't control my fingers to do left/right to quit isearch
    ;; move is still possible with C-f and C-b
    ("<left>" . ivy-done)
    ("<right>" . ivy-done))

  (:map isearch-mode-map
    ("M-S-s" . swiper-isearch-toggle))

  :config
  ;; ivy minibuffer have only 1 line (to mimic isearch = no windows movement)
  (add-to-list 'ivy-height-alist '(swiper-isearch . 1))

  (defun swiper-C-r (&optional arg)
    "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
    (interactive "p")
    (if (string= ivy-text "")
      (ivy-previous-history-element 1)
      (ivy-previous-line arg)))

  ;; add remove from history search when quit with C-g or equivalent
  (advice-add 'swiper--isearch-unwind :after
    (lambda ()
      (unless (eq ivy-exit 'done)
        (swiper--remember-search-history (ivy--regex ivy-text)))))
  ) ;; (use-package swiper

;; outside of use-package otherwise, it does not work well
(global-set-key (kbd "C-S-s") #'isearch)
(global-set-key (kbd "C-S-r") #'isearch-backward)


(provide '02-mode-011-swiper)

;;; 02-mode-011-swiper.el ends here
