;;; shortcut-grep.el --- a config file for grep shortcut

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
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
;; Version: 1.6
;; Created: October 2006
;; Last-Updated: November 2012

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-grep'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-11-26 (1.6)
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
;;
;; to compile
(if section-mode-cedet-ecb
  (global-set-key       (kbd "<f10>")           '(lambda ()
                                                   (interactive)
                                                   (compile compile-command)
                                                   (ecb-goto-window-compilation)
                                                   (end-of-buffer)
                                                   ))
  (global-set-key       (kbd "<f10>")           'compile)
  ) ; (if section-mode-cedet-ecb

;; search all occurrences in the current buffer
;; (more like modern graphical editor)
(global-set-key         (kbd "C-c e")           'occur)

;; search a file in a directory (recursively) to open it
(global-set-key         (kbd "M-f")             'find-name-dired)

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
  ;; alias to replace grep
  (defalias             'grep                   'ack)
  )


(provide 'shortcut-grep)

;;; shortcut-grep.el ends here
