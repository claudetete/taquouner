;;; 02-mode-025-outline.el --- configuration of outline mode

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
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.to manually hide some block in code source]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package outline
  :init
  (outline-minor-mode 1)

  ;; [VARCOMMENT.HIDE ALL AT START: hide all when opening file]
  ;; [VARIABLE.tqnr-section-mode-outline-hide-all-at-start nil]
  (when tqnr-section-mode-outline-hide-all-at-start
    (hide-sublevels 1)))

(when (not tqnr-section-mode-hydra-outline)
  (use-package outline
    :bind
    ;; bind toggle hide/show block
    ("C-c h" . outline-toggle-children)))


(provide '02-mode-025-outline)

;;; 02-mode-025-outline.el ends here
