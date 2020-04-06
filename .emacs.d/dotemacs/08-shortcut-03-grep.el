;;; 08-shortcut-03-grep.el --- a config file for grep shortcut

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
;; Version: 2.4
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage grep]
;; [SUBDEFAULT.t]


;;; Code:

(when (not tqnr-section-mode-hydra-search)
  ;; next occurrence of grep
  (global-set-key         (kbd "<f3>")            'next-match)
  ;;
  ;; previous occurrence of grep
  (global-set-key         (kbd "<S-f3>")          'previous-error)
  ) ;; (when (not tqnr-section-mode-hydra-search)

;; search all occurrences in the current buffer
;; (more like modern graphical editor)
(global-set-key         (kbd "C-c e")           'occur)

;; search a file in a directory (recursively) to open it
(global-set-key         (kbd "C-c C-f")         'find-name-dired)

;; highlight all occurrences (regexp)
(global-set-key         (kbd "C-c x")           'highlight-regexp)


(provide '08-shortcut-03-grep)

;;; 08-shortcut-03-grep.el ends here
