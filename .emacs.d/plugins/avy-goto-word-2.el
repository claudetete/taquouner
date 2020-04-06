;;; avy-goto-word-2.el --- Extension for avy.el. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Claude Tete

;; Author: Claude Tete <claude.tete@gmail.com>
;; URL: https://github.com/claudetete/taquouner
;; Version: 0.1
;; Package-Requires: ((avy "0.4.0"))
;; Keywords: point, location

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; With Avy, you can move point to using word with first letter, but it
;; result in too many. So here an implementation using word with first
;; two letters to restrict results.

;;; Code:
(require 'avy)

;;;###autoload
(defun avy-goto-word-2 (char1 char2 &optional arg beg end symbol)
  "Jump to the currently visible CHAR1 followed by CHAR2 at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg
                     nil nil nil))
  (avy-with avy-goto-word-2
    (let* ((regex1 (avy--build-word-regex char1 symbol))
            (regex2 (avy--build-word-regex char2 symbol t)))
      (avy-jump
        (concat regex1 regex2)
        :window-flip arg
        :beg beg
        :end))))

(defun avy--build-word-regex (char &optional symbol second)
  "Return regexp string to search CHAR as beginning of word.
When SECOND is non-nil, CHAR is considered as second character of word.
When SYMBOL is non-nil, jump to symbol start instead of word start."
  (let* ((str (string char))
          (regex (cond ((string= str ".")
                         "\\.")
                   ((and avy-word-punc-regexp
                      (string-match avy-word-punc-regexp str))
                     (regexp-quote str))
                   ((<= char 26)
                     str)
                   (second
                     str)
                   (t
                     (concat
                       (if symbol "\\_<" "\\b")
                       str)))))
    regex))

;;;###autoload
(defun avy-goto-word-2-above (char1 char2 &optional arg)
  "Jump to the currently visible CHAR1 followed by CHAR2 at a word start.
This is a scoped version of `avy-goto-word-2', where the scope is
the visible part of the current buffer up to point. "
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-2
    (avy-goto-word-2 char1 char2 arg (window-start) (point))))

;;;###autoload
(defun avy-goto-word-2-below (char1 char2 &optional arg)
  "Jump to the currently visible CHAR1 followed by CHAR2 at a word start.
This is a scoped version of `avy-goto-word-2', where the scope is
the visible part of the current buffer following point. "
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-2
    (avy-goto-word-2 char1 char2 arg (point) (window-end (selected-window) t))))


(provide 'avy-goto-word-2)

;;; avy-goto-word-2.el ends here
