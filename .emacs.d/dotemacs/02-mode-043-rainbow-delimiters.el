;;; 02-mode-043-rainbow-delimiters.el --- configuration of rainbow delimiters mode -*- lexical-binding: t -*-

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
;; [SUBHEADER.highlight nested parentheses, brackets in different color depending of depth]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package rainbow-delimiters
  :config
  ;; set dark background
  (setq-default frame-background-mode 'dark)

  :hook
  ;; enable this mode in programming mode
  (prog-mode . rainbow-delimiters-mode)
  ) ;; (use-package rainbow-delimiters


(provide '02-mode-043-rainbow-delimiters)

;;; 02-mode-043-rainbow-delimiters.el ends here
