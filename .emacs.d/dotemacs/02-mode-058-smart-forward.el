;;; 02-mode-058-smart-forward.el --- configuration of smart forward mode

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
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.move in code with semantic see example in plugins/smart-forward.el]
;; [SUBDEFAULT.nil]


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/expand-region"))
(when (try-require 'expand-region "    ")
  (try-require 'smart-forward "    "))


;; c mode
(add-hook 'c-mode-common-hook
  (lambda ()
    ;; set smart-forward mode shortcut
    (local-set-key      (kbd "<M-up>")          'smart-up)
    (local-set-key      (kbd "<M-down>")        'smart-down)
    (local-set-key      (kbd "<M-left>")        'smart-backward)
    (local-set-key      (kbd "<M-right>>")      'smart-forward)))
;; perl mode
(add-hook 'cperl-mode-hook
  (lambda ()
    ;; set smart-forward mode shortcut
    (local-set-key      (kbd "<M-up>")          'smart-up)
    (local-set-key      (kbd "<M-down>")        'smart-down)
    (local-set-key      (kbd "<M-left>")        'smart-backward)
    (local-set-key      (kbd "<M-right>>")      'smart-forward)))
;; python mode
(add-hook 'python-mode-hook
  (lambda ()
    ;; set smart-forward mode shortcut
    (local-set-key      (kbd "<M-up>")          'smart-up)
    (local-set-key      (kbd "<M-down>")        'smart-down)
    (local-set-key      (kbd "<M-left>")        'smart-backward)
    (local-set-key      (kbd "<M-right>>")      'smart-forward)))


(provide '02-mode-058-smart-forward)

;;; 02-mode-058-smart-forward.el ends here
