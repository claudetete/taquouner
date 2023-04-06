;;; 02-mode-106-eglot.el --- configuration of eglot -*- lexical-binding: t -*-

;; Copyright (c) 2023 Claude Tete
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
;; Created: March 2023
;; Last-Updated: March 2023

;;; Commentary:
;;
;; [SUBHEADER.eglot config]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package eglot
  :config
  ;; [VARCOMMENT.Use eglot to manage ada-mode]
  ;; [VARIABLE.tqnr-section-mode-eglot-ada nil]
  (when tqnr-section-mode-eglot-ada
    (add-hook 'ada-mode-hook 'eglot-ensure))
  ;; [VARCOMMENT.Use eglot to manage python-mode]
  ;; [VARIABLE.tqnr-section-mode-eglot-python nil]
  (when tqnr-section-mode-eglot-python
    (add-hook 'python-mode-hook 'eglot-ensure))
  ) ;; (use-package eglot


(provide '02-mode-106-eglot)

;;; 02-mode-106-eglot.el ends here
