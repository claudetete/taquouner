;;; 02-mode-113-consult.el --- configuration of consult -*- lexical-binding: t -*-

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
;; [SUBHEADER.consult: command with filtering like completions]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package consult
  :ensure t)

(use-package consult-projectile
  :ensure t)

(use-package consult-eglot
  :ensure t)



(provide '02-mode-113-consult)

;;; 02-mode-113-consult.el ends here
