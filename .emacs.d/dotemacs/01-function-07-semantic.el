;;; 01-function-07-semantic.el --- add some function about semantic mode

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
;; [SUBHEADER.custom function about semantic mode]
;; [SUBDEFAULT.t]
;;


;;; Code:

;; only when cedet semantic is used
(when tqnr-section-mode-cedet-semantic
 (defvar semantic-tags-location-ring (make-ring 20))
  ;;
  ;;; got to the definition and put it in a memory ring (by Roberto E. Vargas Caballero)
  (defun semantic-goto-definition (point)
    "Goto definition using semantic-ia-fast-jump
   save the pointer marker if tag is found"
    (interactive "d")
    (condition-case err
      (progn
        (ring-insert semantic-tags-location-ring (point-marker))
        (let* ((ctxt (semantic-analyze-current-context point))
                (pf (and ctxt (reverse (oref ctxt prefix))))
                (first (car pf)))
          (if (semantic-tag-p first)
            (semantic-ia--fast-jump-helper first)
            (semantic-complete-jump))))
      (error
        ;;if not found remove the tag saved in the ring
        (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
        (signal (car err) (cdr err)))))
  ;;
  ;;; go back with memory ring  (by Roberto E. Vargas Caballero)
  (defun semantic-pop-tag-mark ()
    "popup the tag save by semantic-goto-definition"
    (interactive)
    (if (ring-empty-p semantic-tags-location-ring)
      (message "%s" "No more tags available")
      (let* ((marker (ring-remove semantic-tags-location-ring 0))
              (buff (marker-buffer marker))
              (pos (marker-position marker)))
        (if (not buff)
          (message "Buffer has been deleted")
          (switch-to-buffer buff)
          (goto-char pos)))))
  ) ; (when tqnr-section-mode-cedet-semantic


(provide '01-function-07-semantic)

;;; 01-function-07-semantic.el ends here
