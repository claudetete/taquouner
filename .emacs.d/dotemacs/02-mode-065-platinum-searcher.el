;;; 02-mode-065-platinum-searcher.el --- configuration of platinum searcher mode -*- lexical-binding: t -*-

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.A front-end for pt, The Platinum Searcher (faster than ack)]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package pt
  :bind
  ("<M-f3>" . pt-regexp)

  :config
  ;; [VARCOMMENT.path to pt executable]
  ;; [VARIABLE.tqnr-profile-mode-platinum-searcher-exec "pt"]
  (setq pt-executable tqnr-profile-mode-platinum-searcher-exec)
  (when tqnr-section-mode-helm
    (use-package helm-ag
      :bind
      ("<M-f3>" . helm-do-ag)

      :custom
      ;; use thing at point to get default value
      (helm-ag-insert-at-point 'symbol)
      ;; use platinum search with helm-ag mode
      (helm-ag-base-command (concat pt-executable " --smart-case -e --nogroup")))
    ) ;; (when tqnr-section-mode-helm
  ) ;; (use-package pt


(provide '02-mode-065-platinum-searcher)

;;; 02-mode-065-platinum-searcher.el ends here
