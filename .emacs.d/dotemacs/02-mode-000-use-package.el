;;; 02-mode-000-use-package.el --- configuration of use-package -*- lexical-binding: t -*-

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
;; Created: April 2020
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Package configuration with simple and tidy macro (mandatory)]
;; [SUBDEFAULT.t]


;;; Code:
(when tqnr-section-environment-package
  (setq package-list '(use-package))

  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
    package-list)
    
  (when (try-require 'use-package-ensure "      ")
    (setq use-package-always-ensure t)))


(provide '02-mode-000-use-package)

;;; 02-mode-000-use-package.el ends here
