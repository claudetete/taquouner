;;; 02-mode-081-all-the-icons.el --- configuration of arduino mode -*- lexical-binding: t -*-

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
;; Created: August 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [[SUBHEADER.mode to have nice icons (from special fonts)
;; install font on your system from `fonts' folder or use
;; M-x all-the-icons-install-fonts
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t)
  ) ;; (use-package all-the-icons


(provide '02-mode-081-all-the-icons)

;;; 02-mode-081-all-the-icons.el ends here
