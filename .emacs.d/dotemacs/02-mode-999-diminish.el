;;; 02-mode-999-diminish.el --- configuration of diminish mode

;; Copyright (c) 2017-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.shrink major and minor mode name in the modeline]
;; [SUBDEFAULT.nil]


;;; Code:
;; must be load after all other modes
(when (try-require 'diminish "    ")
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode))
  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode))
  (eval-after-load "gtags"
    '(diminish 'gtags-mode))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
  (eval-after-load "projectile-mode"
    '(diminish 'projectile-mode))
  (eval-after-load 'company
    '(diminish 'company-mode))
  (eval-after-load 'helm-mode
    '(diminish 'helm-mode))
  (eval-after-load 'helm-gtags-mode
    '(diminish 'helm-gtags-mode))
  (eval-after-load 'smartparens-mode
    '(diminish 'smartparens-mode))
  (eval-after-load 'hs-minor-mode
    '(diminish 'hs-minor-mode))
  (eval-after-load 'iimage-mode
    '(diminish 'iimage-mode))
  (eval-after-load 'pandoc-mode
    '(diminish 'pandoc-mode))
;;  (eval-after-load 'ada-mode
;;    '(diminish 'ada-mode))
  (eval-after-load 'org-mode
    '(diminish 'org-mode))
  (eval-after-load 'auto-revert-mode
    '(diminish 'auto-revert-mode))
  (add-hook 'emacs-lisp-mode-hook
    (lambda()
      (setq mode-name "el")))
  (eval-after-load 'fitnesse-mode
    (lambda()
      (setq mode-name "FiT")))
  )


(provide '02-mode-999-diminish)

;;; 02-mode-999-diminish.el ends here
