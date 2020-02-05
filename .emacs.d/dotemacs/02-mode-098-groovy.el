;;; 02-mode-098-groovy.el --- configuration of groovy mode

;; Copyright (c) 2019 Claude Tete
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
;; Created: August 2019
;; Last-Updated: August 2019

;;; Commentary:
;;
;; [SUBHEADER.groovy mode]
;; [SUBDEFAULT.nil]


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/groovy-emacs-modes-master/"))
(when (try-require 'groovy-mode "    ")
  ;; set groovy-mode for jenkins file
  (add-to-list 'auto-mode-alist '("\\.jf\\'" . groovy-mode))
  (setq groovy-indent-offset 2)
  (add-hook 'groovy-mode-hook
    (lambda ()
      (c-set-offset 'arglist-intro 2)
      (c-set-offset 'label 2)))
  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )

(provide '02-mode-098-groovy)

;;; 02-mode-098-groovy.el ends here
