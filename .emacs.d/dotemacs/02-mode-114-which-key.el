;;; 02-mode-114-which-key.el --- configuration of which-key -*- lexical-binding: t -*-

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
;; [SUBHEADER.which-key: displays key bindings following entered incomplete command]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package which-key
  :init (which-key-mode)

  :config
  (setq which-key-idle-delay 0.3))

(provide '02-mode-114-which-key)

;;; 02-mode-114-which-key.el ends here
