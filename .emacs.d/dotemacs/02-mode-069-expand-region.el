;;; 02-mode-069-expand-region.el --- configuration of expand region mode -*- lexical-binding: t -*-

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Increase selected region by semantic units]
;; [SUBDEFAULT.t]


;;; Code:
(use-package expand-region
  :pin melpa

  :bind
  ;; M-s then s, s, etc to expand selection region
  ("M-s" . er/expand-region)
  ;; M-S to contract expanded selection region
  ("M-S" . er/contract-region)

  :config
  ;; do not use fast key to expand/contract (bind by default to 's' and '-')
  (setq expand-region-fast-keys-enabled nil)
  )


(provide '02-mode-069-expand-region)

;;; 02-mode-069-expand-region.el ends here
