;;; 02-mode-065-platinum-searcher.el --- configuration of platinum searcher mode

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
;; Created: July 2017
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.A front-end for pt, The Platinum Searcher (faster than ack)]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'autoload-pt "    ")
  ;; [VARCOMMENT.path to pt executable]
  ;; [VARIABLE.tqnr-profile-mode-platinum-searcher-exec "pt.exe"]
  (setq pt-executable tqnr-profile-mode-platinum-searcher-exec)
  (when tqnr-section-mode-helm
    (try-require 'autoload-helm-ag "      ")
    (custom-set-variables
      ;; use thing at point to get default value
      '(helm-ag-insert-at-point 'symbol)
      ;; use platinum search with helm-ag mode
      '(helm-ag-base-command "pt --smart-case -e --nogroup"))
    )
  )


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (if tqnr-section-mode-helm
      (global-set-key   (kbd "<M-f3>")  'helm-do-ag)
      (global-set-key   (kbd "<M-f3>")  'pt-regexp)
      )
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-065-platinum-searcher)

;;; 02-mode-065-platinum-searcher.el ends here
