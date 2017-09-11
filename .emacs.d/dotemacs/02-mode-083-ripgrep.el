;;; 02-mode-083-ripgrep.el --- configuration of ripgrep mode with helm

;; Copyright (c) 2017 Claude Tete
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
;; Created: September 2017
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [SUBHEADER.A front-end for rg, ripgrep (faster than anything...)]

;;; Change Log:
;; 2017-09-01 (0.1)
;;    creation from scratch


;;; Code:
(when tqnr-section-mode-helm
  (try-require 'autoload-helm-ag "      ")
  (custom-set-variables
    ;; use thing at point to get default value
    '(helm-ag-insert-at-point 'symbol)
    ;; use platinum search with helm-ag mode
    '(helm-ag-base-command "rg --smart-case --no-heading --line-number"))

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      (global-set-key   (kbd "<M-f3>")  'helm-do-ag)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )





(provide '02-mode-083-ripgrep)

;;; 02-mode-083-ripgrep.el ends here
