;;; 02-mode-090-flex-isearch.el --- configuration of flex-isearch

;; Copyright (c) 2017-2020 Claude Tete
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
;; Created: December 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Flex Isearch mode add fuzzy match when doing incremental search]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package flex-isearch
  :bind
  ("C-S-s" . flx-isearch-forward)
  ("C-S-r" . flx-isearch-backward)

  :init
  (global-flex-isearch-mode t)
  :config
  (setq flex-isearch-auto "Never")
  ) ;; (use-package flex-isearch


(provide '02-mode-090-flex-isearch)

;;; 02-mode-090-flex-isearch.el ends here
