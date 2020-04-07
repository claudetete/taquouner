;;; 02-mode-076-cflow.el --- configuration of cflow mode -*- lexical-binding: t -*-

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
;; [SUBHEADER.useful to have call tree in C source code]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package cflow-mode
  :mode "\\.cflow\\'"

  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/cflow-mingw-master/cflow-1.4/elisp"))
  ) ;; (use-package cflow-mode


(provide '02-mode-076-cflow)

;;; 02-mode-076-cflow.el ends here
