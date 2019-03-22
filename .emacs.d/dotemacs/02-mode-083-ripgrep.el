;;; 02-mode-083-ripgrep.el --- configuration of ripgrep mode with helm

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
;; Created: September 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [[SUBHEADER.A front-end for rg, ripgrep (faster than anything...)
;; use .ripgreprc to add new type
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(when tqnr-section-mode-helm
  ;; (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  ;; (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")))

  (try-require 'autoload-helm-ag "      ")
  (custom-set-variables
    ;; use thing at point to get default value
    '(helm-ag-insert-at-point 'symbol)
    ;; use ripgrep search with helm-ag mode
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
