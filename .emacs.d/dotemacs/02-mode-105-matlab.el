;;; 02-mode-105-matlab.el --- configuration of matlab-mode -*- lexical-binding: t -*-

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
;; Created: October 2020
;; Last-Updated: October 2020

;;; Commentary:
;;
;; [SUBHEADER.Matlab mode config]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package matlab-mode
  :mode "\\.m\\'"

  :config
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab")
  ) ;; (use-package matlab-mode


(provide '02-mode-105-matlab)

;;; 02-mode-105-matlab.el ends here
