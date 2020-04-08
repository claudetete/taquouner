;;; 02-mode-003-doxymacs.el --- configuration of doxymacs mode -*- lexical-binding: t -*-

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
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.emacs interface for doxygen comments]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package doxymacs
  :hook (c-mode-common-hook . doxymacs-mode)
  :config
  (defvar doxymacs-doxygen-style "JavaDoc")
  (defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook #'my-doxymacs-font-lock-hook))


(provide '02-mode-003-doxymacs)

;;; 02-mode-003-doxymacs.el ends here
