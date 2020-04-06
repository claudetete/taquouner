;;; 02-mode-018-dired-plus.el --- configuration of dired+ mode

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.improve Dired mode, color, open with, etc]
;; [SUBDEFAULT.t]


;;; Code:
(use-package dired+
  :bind (:map dired-mode-map
          ;; open with default associated application
          ("H-RET" . dired-w32-browser)
          ;; was dired-advertised-find-file
          ("RET" . dired-find-alternate-file)
          ;; was dired-up-directory
          ("[" . (lambda () (interactive) (find-alternate-file "..")))
          ;;("[" . dired-up-directory)
          ;; edit buffer of dired to modify filename by example (C-x C-s or C-c C-c to apply modification)
          ("C-c C-e" . dired-toggle-read-only)
          )

  :init
  ;; to have only one dired buffer by dired instance
  (toggle-diredp-find-file-reuse-dir t))


(provide '02-mode-018-dired-plus)

;;; 02-mode-018-dired-plus.el ends here
