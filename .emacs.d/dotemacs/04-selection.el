;;; 04-selection.el --- a config file for text selection settings -*- lexical-binding: t -*-

;; Copyright (c) 2006-2019 Claude Tete
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
;; Version: 1.8
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [HEADER.selection can be kill + selection is highlight + kill->copy in read only]
;; [DEFAULT.t]


;;; Code:
;; selection can be killed/deleted
(delete-selection-mode t)
;;
;; selection is highlighted
(setq transient-mark-mode t)

(if (or tqnr-running-on-emacs-23 (and tqnr-running-on-emacs-24 (not tqnr-running-on-emacs-24-4) (not tqnr-running-on-emacs-24-5)))
  ;; to have rectangle mode (not very compatible with other mode)
  (cua-selection-mode t))
;; bug with enabled dynamic completion, it insert 0x00

;; read under

;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)

;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-l] downcases the rectangle
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)

;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].

;;
;; [VARCOMMENT.SHIFT SELECTION]
;; [VARIABLE.tqnr-section-selection-with-shift nil]
(if tqnr-section-selection-with-shift
  (progn
    (message "    Selection with Shift...")
    ;; selection with shift (default since Emacs 23.4)
    (setq shift-selection-mode t)
    (message "    Selection with Shift... Done")
    )
  ;; try to disable it
  (progn (setq shift-selection-mode nil))
  )

;; read only buffer can be put in kill ring without delete characters
(setq kill-read-only-ok t)


(provide '04-selection)

;;; 04-selection.el ends here
