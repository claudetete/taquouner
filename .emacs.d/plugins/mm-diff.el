;;; mm-diff.el --- generic mode to coloration of synergy diff file

;; Copyright (c) 2011, 2012 Claude Tete
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

;; Keywords: mode, coloration, diff, synergy, telelogic, ibm
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: June 2011
;; Last-Updated: June 2012

;;; Commentary:
;; only colors

;;; Change Log:
;; 2012-06-05 (1.1)
;;    update header + add provide
;; 2011-06-24 (1.0)
;;    fix some bug
;; 2011-06-17 (0.1)
;;    creation from scratch

(require 'generic-x) ;; we need this

(define-generic-mode
  ;; name
  'mm-diff-mode

  ;; comments start with '//'
  '("^---")

  ;; keywords
  '("Attribute name" "create_time" "modify_time" "owner    " "release    " "status    " "version    " "binary_scan_file_time" "comment    " "source_create_time" "source_modify_time" "Associated tasks differs" "Text attribute 'cluster_id' differs" "Text attribute 'source' differs" "Text attribute 'status_log' differs"
     )

  '(
     ;; '=' is an operator
;     ("=" . 'font-lock-operator)

     ;; ';' is a a built-in
;     (";" . 'font-lock-builtin)

     ;; header color
     ("^-------------------------" . 'font-lock-type-face)
     ("--------------------------$" . 'font-lock-function-name-face)
     (" -------------------------- " . 'font-lock-builtin-face)

     ;; compare separation "line before c line after"
     ("^[0-9]+\\(,[0-9]+\\)?c[0-9]+\\(,[0-9]+\\)?$" . 'font-lock-type-face)

     ;; add separation "line before a line after"
     ("^[0-9]+\\(,[0-9]+\\)?a[0-9]+\\(,[0-9]+\\)?$" . 'font-lock-variable-name-face)

     ;; delete separation "line before a line after"
     ("^[0-9]+\\(,[0-9]+\\)?d[0-9]+\\(,[0-9]+\\)?$" . 'font-lock-comment-face)


     ;; After
     ("^> .*$" . 'font-lock-function-name-face)


     ;; Before
     ("^< .*$" . 'font-lock-builtin-face)

     )

  ;; files for which to activate this mode
  '("_[A-Z][A-Z][A-Z]\\.txt$")

  ;; other functions to call
  nil

  ;; doc string for this mode
  "A mode for Magneti Marelli synergy diff files"
  )


(provide 'mm-diff)

;;; mm-diff.el ends here
