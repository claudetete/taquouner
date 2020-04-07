;;; 02-mode-098-groovy.el --- configuration of groovy mode

;; Copyright (c) 2019-2020 Claude Tete
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
;; Version: 0.2
;; Created: August 2019
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.groovy mode]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package groovy-mode
  ;; set groovy-mode for jenkins file
  :mode "\\.jf\\'"

  :config
  (setq groovy-indent-offset 2)

  :hook (groovy-mode-hook .
          (lambda ()
            (c-set-offset 'arglist-intro 2)
            (c-set-offset 'label 2)))
  ) ;; (use-package groovy-mode


(provide '02-mode-098-groovy)

;;; 02-mode-098-groovy.el ends here
