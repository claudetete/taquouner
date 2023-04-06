;;; 02-mode-108-vertico.el --- configuration of vertico -*- lexical-binding: t -*-

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
;; [SUBHEADER.vertico: vertical list for completions]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package vertico
  :ensure t

  :init
  (vertico-mode)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow only (never shrink) the Vertico minibuffer
  (setq vertico-resize 'grow-only)

  :bind (:map vertico-map
              ("<escape>" . minibuffer-keyboard-quit)
              ;; reproduce helm behavior
              ("<left>" . vertico-directory-up)
              ("<right>" . vertico-insert))
  )

(use-package vertico-directory
  :after vertico
  :ensure nil

  :bind (:map vertico-map
              ("<left>" . vertico-directory-up)
              ("<right>" . vertico-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(provide '02-mode-108-vertico)

;;; 02-mode-108-vertico.el ends here
