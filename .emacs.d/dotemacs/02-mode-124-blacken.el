;;; 02-mode-124-blacken.el --- configuration of blacken -*- lexical-binding: t -*-

;; Copyright (c) 2024 Claude Tete
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
;; Created: February 2024
;; Last-Updated: February 2024

;;; Commentary:
;;
;; [SUBHEADER.blacken: mode for black, the uncompromising Python code formatter]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package blacken
  :after python
  :hook (python-mode . blacken-mode)

  :config
  (setq blacken-allow-py36 t)
  (setq blacken-skip-string-normalization t)
  (setq blacken-line-length 120)
  )

(provide '02-mode-124-blacken)

;;; 02-mode-124-blacken.el ends here
