;;; 02-mode-112-corfu.el --- configuration of corfu -*- lexical-binding: t -*-

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
;; [SUBHEADER.corfu: autocompletion like company - overlay list]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-cycle t) ; cycle around to first entry after reaching the last
  (corfu-preview-current nil) ; don't expand text at point until I press return
  (corfu-min-width 20)
  (corfu-on-exact-match 'insert) ; complete if there is only a single candidate
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-auto t)
  (corfu-separator ?\s)

  :config
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation next to completions

  )

;; (use-package kind-icon
;;   :after corfu
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; fix overly large icons (https://github.com/jdtsmith/kind-icon/issues/22)
;; (setq kind-icon-default-style
;;       '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.4 :scale 1))

;; (use-package cape
;;   :defer 1
;;   :config
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev 20) ; words from buffer
;;   (add-hook 'completion-at-point-functions #'cape-file 20))

(provide '02-mode-112-corfu)

;;; 02-mode-112-corfu.el ends here
