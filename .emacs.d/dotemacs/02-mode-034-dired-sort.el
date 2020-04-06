;;; 02-mode-034-dired-sort.el --- configuration of dired sort mode

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
;; [SUBHEADER.more option to sort in Dired mode]
;; [SUBDEFAULT.t]


;;; Code:
(use-package dired-sort-menu
  :custom
  ;; dired is allow to delete recursively folder
  (dired-recursive-copies t)
  (dired-recursive-deletes t)
  ;; set a profile of sorting
  (dired-sort-menu-saved-config
    (quote (
             (dired-actual-switches . "-alh")
             (ls-lisp-ignore-case . t)
             (ls-lisp-dirs-first . t))))
  ;; set this profile by default
  (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case t)
  ) ;; (use-package dired-sort-menu


(provide '02-mode-034-dired-sort)

;;; 02-mode-034-dired-sort.el ends here
