;;; 02-mode-046-fold-dwim.el --- configuration of fold dwim mode

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
;; [SUBHEADER.show hide code source block]
;; [SUBDEFAULT.t]


;;; Code:
(use-package fold-dwim
  :bind  (
           ;; only in C mode
           :map c-mode-map
           ;; show/hide block
           ("<H-left>"  . fold-dwim-toggle)
           ("<H-right>" . fold-dwim-toggle)
           ;; hide all
           ("<H-up>"    . fold-dwim-hide-all)
           ;; show all
           ("<H-down>"  . fold-dwim-show-all)

           :map rtrt-script-mode-map
           ;; hide all
           ("<H-prior>" . fold-dwim-hide-all)
           ;; show all
           ("<H-next>"  . fold-dwim-show-all)
           )
  ) ;; (use-package fold-dwim


(provide '02-mode-046-fold-dwim)

;;; 02-mode-046-fold-dwim.el ends here
