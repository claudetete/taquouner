;;; 02-mode-063-hide-lines.el --- configuration of hide lines mode

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
;; [SUBHEADER.hide lines using regexp (like narrow but with regex and not region)]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package hide-lines
  :bind
  ;; hide match
  ("C-c l h" . hide-lines-matching)
  ;; hide not match
  ("C-c l n" . hide-lines-not-matching)
  ;; show all
  ("C-c l s" . hide-lines-show-all)
  ) ;; (use-package hide-lines


(provide '02-mode-063-hide-lines)

;;; 02-mode-063-hide-lines.el ends here
