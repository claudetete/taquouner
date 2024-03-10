;;; 02-mode-121-popper.el --- configuration of popper -*- lexical-binding: t -*-

;; Copyright (c) 2023-2024 Claude Tete
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
;; Created: August 2023
;; Last-Updated: January 2024

;;; Commentary:
;;
;; [SUBHEADER.popper: display buffer as popup (popwin alternative)]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package popper
  :ensure t

  :bind (("<f2>" . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))

  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          helpful-mode
          help-mode
          compilation-mode
          magit-status-mode
          "\\*helm-ag\\*"))
  (popper-mode +1)
  (popper-echo-mode +1)

  :config
  ;; only display the popup buffer but do no always select it, let others decide it
  (setq popper-display-function #'popper-display-popup-at-bottom)

  (when tqnr-section-mode-shackle
    (setq popper-display-control nil))
  ) ; (use-package popper


(provide '02-mode-121-popper)

;;; 02-mode-121-popper.el ends here
