;;; 02-mode-090-flex-isearch.el --- configuration of flex-isearch

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
;; Created: December 2017
;; Last-Updated: December 2017

;;; Commentary:
;;
;; [SUBHEADER.Flex Isearch mode add fuzzy match when doing incremental search]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-12-15 (0.1)
;;    creation from scratch


;;; Code:

(when (try-require 'flex-isearch "    ")
  (setq flex-isearch-auto "Never")
  (global-flex-isearch-mode t)
  )


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (global-set-key (kbd "C-S-s") #'flx-isearch-forward)
    (global-set-key (kbd "C-S-r") #'flx-isearch-backward)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook
)


(provide '02-mode-090-flex-isearch)

;;; 02-mode-090-flex-isearch.el ends here
