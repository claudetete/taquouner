;;; 02-mode-123-alert.el --- configuration of alert -*- lexical-binding: t -*-

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
;; Created: January 2024
;; Last-Updated: January 2024

;;; Commentary:
;;
;; [SUBHEADER.alert: manage desktop alert/notification/toast]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package alert
  :commands (alert)
  :config (setq alert-default-style 'toast))

(use-package alert-toast
  :after alert)

(provide '02-mode-123-alert)

;;; 02-mode-123-alert.el ends here
