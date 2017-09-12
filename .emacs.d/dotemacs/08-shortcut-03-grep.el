;;; 08-shortcut-03-grep.el --- a config file for grep shortcut

;; Copyright (c) 2006-2017 Claude Tete
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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.4
;; Created: October 2006
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage grep]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-09-11 (2.4)
;;    add condition about same hydra search shortcut not very useful here when
;;    helm is used
;; 2017-07-26 (2.3)
;;    update to new conf format
;; 2017-05-26 (2.2)
;;    use helm with projectile if used instead of patched gtags + add avy mode
;; 2016-09-28 (2.1)
;;    add platinium search (+helm) shortcut
;; 2014-03-26 (2.0)
;;    change ace jump shortcut and find-name-dired function
;; 2013-04-10 (1.9)
;;    add helm mode shortcut
;; 2012-12-04 (1.8)
;;    remove compile shortcut
;; 2012-11-29 (1.7)
;;    add ace jump shortcut
;; 2012-11-27 (1.6)
;;    add ack mode shortcut
;; 2012-10-18 (1.5)
;;    add open/close compil window with ecb when try to compile
;; 2012-08-01 (1.4)
;;    use f3 for all next/previous, move search bind from other shortcut file
;; 2012-03-30 (1.3)
;;    translate comments in English
;; 2012-03-22 (1.2)
;;    add condition about working environment
;; 2011-07-27 (1.1)
;;    change shortcut for grep-find by project
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


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
