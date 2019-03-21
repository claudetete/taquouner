;;; ada-mode-expansions.el --- ada-specific expansions for expand-region

;; Copyright (C) 2018 Claude Tete

;; Author: Claude Tete <claude.tete@gmail.com>
;; Keywords: marking region ada

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'expand-region-core)

(defun er/mark-ada-symbol-with-attribute ()
  "Marks one ada symbol"
  (interactive)
  (er/mark-method-call)
  (when (use-region-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (while (er/looking-back-exact ".")
      (backward-char 1)
      (skip-syntax-backward "_w"))
    (exchange-point-and-mark)
    (while (looking-at "'")
      (forward-char 1)
      (skip-syntax-forward "_w"))
    (exchange-point-and-mark)))

(defun er/mark-ada-call-function-or-array ()
  "Marks one ada call function or array access"
  (interactive)
  (er/mark-method-call)
  (when (use-region-p)
    (when (> (mark) (point))
      (exchange-point-and-mark))
    (let ((oldpos (point)))
      (skip-chars-forward "\s\n")
      (if (looking-at "(")
        (progn (forward-sexp)
          (exchange-point-and-mark))
        (goto-char oldpos)))))

(defun er/mark-ada-if ()
  "Mark block of if"
  (interactive)
  (er/mark-method-call)
  (re-search-backward "^\\( \\)*\\bif\\b")
  (set-mark (point))
  (skip-chars-forward " ")
  (while (not (er/looking-back-exact "end if"))
    (forward-sexp))
  (end-of-line)
  (forward-char)
  (exchange-point-and-mark))

(defun er/mark-ada-loop ()
  "Mark block of loop"
  (interactive)
  (er/mark-method-call)
  (re-search-backward "^\\( \\)*\\b\\(for\\|loop\\)\\b")
  (set-mark (point))
  (skip-chars-forward " ")
  (while (not (er/looking-back-exact "end loop"))
    (forward-sexp))
  (end-of-line)
  (forward-char)
  (exchange-point-and-mark))

(defun er/mark-ada-loop ()
  "Mark block of loop"
  (interactive)
  (er/mark-method-call)
  (re-search-backward "^\\( \\)*\\b\\(for\\|loop\\)\\b")
  (set-mark (point))
  (skip-chars-forward " ")
  (while (not (er/looking-back-exact "end loop"))
    (forward-sexp))
  (end-of-line)
  (forward-char)
  (exchange-point-and-mark))

(defun er/mark-ada-function-or-procedure ()
  "Mark whole function or procedure"
  (interactive)
  (ada-goto-declaration-start)
  (beginning-of-line)
  (set-mark (point))
  (skip-chars-forward " ")
  (ada-goto-declaration-end)
  (end-of-line)
  (forward-char)
  (exchange-point-and-mark))

(defun er/add-ada-mode-expansions ()
  "Add ada mode expansinos"
  (set (make-local-variable 'er/try-expand-list) (append er/try-expand-list
                                                   '(
                                                      er/mark-ada-symbol-with-attribute
                                                      er/mark-ada-call-function-or-array
                                                      er/mark-ada-if
                                                      er/mark-ada-loop
                                                      er/mark-ada-function-or-procedure
                                                      ))))

(er/enable-mode-expansions 'ada-mode 'er/add-ada-mode-expansions)

(provide 'ada-mode-expansions)

;; ada-mode-expansions.el ends here
