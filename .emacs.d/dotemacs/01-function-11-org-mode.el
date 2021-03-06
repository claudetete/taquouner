;;; 01-function-11-org-mode.el --- add some function about org-mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2019 Claude Tete
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
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about org-mode]
;; [SUBDEFAULT.nil]
;;


;;; Code:

;; ;;
;; (defun tqnr-function-on-each-lines (func)
;;   "Call FUNCTION on each line of region."
;;   ;; declare variable
;;   (let (end)
;;     (save-excursion
;;       ;; go to end of region
;;       (goto-char (region-end))
;;       (if (not (bolp))
;;         (forward-line 1))
;;       ;; go to begin of current line
;;       (beginning-of-line)
;;       ;; store last line marker
;;       (setq end (copy-marker (point-marker)))
;;       ;; go to begin of region
;;       (goto-char (region-beginning))
;;       ;; go to begin of current line
;;       (beginning-of-line)
;;       ;; loop on each line of region
;;       (while (< (point) end)
;;         ;; apply
;;         (funcall func)
;;         ;; go to next line
;;         (forward-line 1)
;;         )
;;       )
;;     )
;;   )

(defun org-shiftright-dwim ()
  "Org Shift Right on current line or all line of region."
  (interactive)
  (if (use-region-p)
    (tqnr-function-on-each-lines 'org-shiftright)
    (org-shiftright)
    )
  )

(defun org-shiftleft-dwim ()
  "Org Shift Left on current line or all line of region."
  (interactive)
  (if (use-region-p)
    (tqnr-function-on-each-lines 'org-shiftleft)
    (org-shiftleft)
    )
  )

;; (defun org-todo-dwim ()
;;   "Org TODO on current line or all line of region."
;;   (interactive)
;;   (if (use-region-p)
;;     (tqnr-function-on-each-lines 'org-todo)
;;     (progn
;;       (org-todo)
;;       (hide-subtree))))

(defun my/mark-todo-in-region ()
  (interactive)
  (let ((scope (if mark-active 'region 'tree))
        (state (org-fast-todo-selection)))
    (org-map-entries (lambda () (org-todo state)) nil scope)))

(provide '01-function-11-org-mode)

;;; 01-function-11-org-mode.el ends here
