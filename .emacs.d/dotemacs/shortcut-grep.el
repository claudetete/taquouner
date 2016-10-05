;;; shortcut-grep.el --- a config file for grep shortcut

;; Copyright (c) 2006-2016 Claude Tete
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

;; Keywords: config, shortcut, grep
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.1
;; Created: October 2006
;; Last-Updated: September 2016

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-grep'
;;              var     `section-shortcut'

;;; Change Log:
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
(when section-function-mm
  ;; custom grep for NBNF LL project
  (global-set-key       "\C-cnn"                'nll-grep-find)
  ;; custom grep for NBNF HL project
  (global-set-key       "\C-cnh"                'nhl-grep-find)
  ;; custom grep for NSF project
  (global-set-key       "\C-cns"                'nsf-grep-find)
  ;; custom grep for ENSF project
  (global-set-key       "\C-cne"                'ecar-grep-find)
  ) ; (when section-function-mm

;; next occurrence of grep
(global-set-key         (kbd "<f3>")            'next-match)
;;
;; previous occurrence of grep
(global-set-key         (kbd "<S-f3>")          'previous-error)

;; search all occurrences in the current buffer
;; (more like modern graphical editor)
(if section-mode-helm-occur
  (global-set-key       (kbd "C-c e")           'helm-occur)
  (global-set-key       (kbd "C-c e")           'occur))

;; search a file in a directory (recursively) to open it
(global-set-key         (kbd "C-c c-f")         'find-name-dired)

;;
;;; HELM
;; to replace find file by helm find file
(when section-mode-helm-find-files
  (global-set-key       (kbd "C-x C-f")         'helm-find-files))
;;
(when section-mode-helm-recentf
  (global-set-key       (kbd "C-c f")           'helm-recentf))

;; search the word at point (cannot bind C-M-x still run 'eval-defun)
(global-set-key         (kbd "C-M-v")           'isearch-forward-at-point)
;; search all occurences at point
(global-set-key         (kbd "C-M-c")           'occur-word-at-point)
;; occur when incremental search
(define-key isearch-mode-map (kbd "C-o")        'isearch-occur)

;; highlight all occurrences (regexp)
(global-set-key         (kbd "C-c x")           'highlight-regexp)

;;
;;; ACK (replace grep)
(when section-mode-ack-emacs
  ;; run ack with thing at point
  (global-set-key       (kbd "<C-f3>")          'ack)
  ;; run ack with thing at point but only with same file type
  (global-set-key       (kbd "<M-f3>")          'ack-same)
  )

;;
;;; PLATINIUM SEARCH
(when section-mode-platinium-search
  (if section-mode-helm
    (global-set-key     (kbd "<M-f3>")          'helm-do-ag)
    (global-set-key     (kbd "<M-f3>")          'pt-regexp)
    )
  )

;;
;;; ACE JUMP
(when section-mode-ace-jump
  (global-set-key       (kbd "<f12>")            'ace-jump-mode)
  ;; can also use <C-u f9>
  (global-set-key       (kbd "<M-f12>")          'ace-jump-char-mode)
  ;; can also use <C-u C-u f9>
  (global-set-key       (kbd "<C-f12>")          'ace-jump-line-mode)
  (global-set-key       (kbd "<S-f12>")          'ace-jump-mode-pop-mark)
  )

(provide 'shortcut-grep)

;;; shortcut-grep.el ends here
