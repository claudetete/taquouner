;;; 02-mode-088-fitnesse.el --- configuration of fitnesse-mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.2
;; Created: November 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.FitNesse MarkUp files syntax highlight]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package fitnesse-mode
  ;;:demand

  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins"))

  :init
  ;; associate context.txt files to fitnesse-mode
  (add-to-list 'auto-mode-alist '("content\\.txt\\'" . fitnesse-mode))

  ;; force open FitNesse files with utf-8 coding
  (modify-coding-system-alist 'file "content\\.txt\\'" 'utf-8)

  ;; :bind
  ;; (:map fitnesse-mode-map
  ;;   ("C-M-S-i" . iimage-mode))

  :custom
  (fitnesse-pretty-print-executable `(concat (file-name-as-directory (getenv "VIEWPATH")) "bin/fitformat"))
  )

(provide '02-mode-088-fitnesse)

;;; 02-mode-088-fitnesse.el ends here
