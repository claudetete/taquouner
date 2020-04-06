;;; 02-mode-086-dumb-jump.el --- configuration of dumb jump mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Created: September 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.On-the-fly spell checking]
;; [SUBDEFAULT.nil]


;;; Code:

(when (try-require 'dumb-jump "    ")
  (when tqnr-section-mode-ripgrep
    (setq dumb-jump-prefer-searcher 'rg)
    (setq dumb-jump-force-searcher 'rg)
    )
  (when tqnr-section-mode-helm
    (setq dumb-jump-selector 'helm))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; run ispell (dictionary) (set language in `section-misc')
    (global-set-key         (kbd "M-,")             'dumb-jump-go)
    ;;("M-g o" . dumb-jump-go-other-window)
    ;;("M-g j" . dumb-jump-go)
    ;;("M-g i" . dumb-jump-go-prompt)
    ;;("M-g x" . dumb-jump-go-prefer-external)
    ;;("M-g z" . dumb-jump-go-prefer-external-other-window)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook
)

(provide '02-mode-086-dumb-jump)

;;; 02-mode-086-dumb-jump.el ends here
