;;; 01-function-02-select-copy.el --- add some function to select or copy -*- lexical-binding: t -*-

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about copying and selecting buffer text]
;; [SUBDEFAULT.t]
;;


;;; Code:

;;  CONST
(defconst tqnr-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")

;;
;; match a string (by Claude TETE)
(defun tqnr-match-string (n)
  "Match a string (N)."
  (buffer-substring (match-beginning n) (match-end n)))

;;; select the word at point (by Claude TETE)
(defun tqnr-select-word ()
  "Select the word under cursor."
  (cond
    ((looking-at "[0-9A-Za-z_]")
      (while (and (not (bolp)) (looking-at "[0-9A-Za-z_]"))
        (forward-char -1))
      (if (not (looking-at "[0-9A-Za-z_]")) (forward-char 1)))
    (t
      (while (looking-at "[ \t]")
        (forward-char 1))))
  (if (looking-at tqnr-symbol-regexp)
    (tqnr-match-string 0) nil)
  )

;; TO BE REPLACE by word-at-point
;; interactive select word at point (by Claude TETE)
(defun select-word-under ()
  "Select the word under cursor."
  (interactive)
  (let (pt)
    (when (boundp 'auto-highlight-symbol-mode)
      (ahs-clear))
    (skip-chars-backward "_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "_A-Za-z0-9")
    (set-mark pt)
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; select the whole word at point
    (global-set-key     (kbd "C-M-z")   'select-word-under)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;; get the string from word at point or region
(defun tqnr-get-string-position ()
  (let ()
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (list (car (bounds-of-thing-at-point 'word)) (cdr (bounds-of-thing-at-point 'word)))
      )
    )
  )

;; get the string from line at point or region
(defun tqnr-get-line-position ()
  (let ()
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (list (line-beginning-position) (line-beginning-position 2))
      )
    )
  )

;; get the string from line at point or region
(defun tqnr-get-paragraph-position ()
  (let ()
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (let ((bds (bounds-of-thing-at-point 'paragraph)) )
        (list (car bds) (cdr bds)))
      )
    )
  )

;;
;; COPY/CUT
;; Change cutting behavior (from http://emacswiki.org/emacs/WholeLineOrRegion):
(when (not tqnr-section-mode-easy-kill)
  (put 'kill-ring-save 'interactive-form
    '(interactive
       (if (use-region-p)
         (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))
  )

;; easy-kill does not implement this way
(put 'kill-region 'interactive-form
  '(interactive
     (if (use-region-p)
       (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))


;; Copy entire line in kill ring (without Home C-k C-y) (by Claude TETE)
(defun push-line ()
  "Select current line, push onto kill ring."
  (interactive)
  (save-excursion
    (copy-region-as-kill (re-search-backward "^") (re-search-forward "$"))
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; copy line in kill-ring `C-j' (no more C-k C-y)
    (global-set-key     (kbd "C-j")             'push-line)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(when (not tqnr-section-mode-easy-kill)
  ;; from http://www.emacswiki.org/emacs/UnifyKillringAndRegister
  ;; [2006/02/10] kill-ring / register
  (defun kill-ring-save-x (s e)
    (interactive "r")
    (if (eq last-command 'kill-ring-save-x)
        (call-interactively 'copy-to-register)
      (call-interactively 'kill-ring-save)))
  (define-key esc-map "w" 'kill-ring-save-x)
  ;;;; [2006/02/25] kill-region / register
  ;;(defun kill-region-x (s e)
  ;; (interactive "r")
  ;;  (if (eq last-command 'kill-region)    ;kill-region-x ?
  ;;      (call-interactively 'my-kill-ring-to-register)
  ;;    (call-interactively 'kill-region)))
  ;;(defun my-kill-ring-to-register (register)
  ;;  (interactive "cCopy to register: ")
  ;;  (set-register register (car kill-ring)))
  ;;(define-key global-map "\C-w" 'kill-region-x)
  )


;; like `exchange-point-and-mark' (C-x C-x) but after action on rectangle
;; and do not have unpop function (must implement a unpop mark ring)
;; helm-mark-ring was no help, always put at beginning of line of mark...
(defun pop-local-mark ()
  "Do like C-u C-SPC but saving current point in mark ring"
  (interactive)
  (set-mark-command t))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; remap C-u C-SPC more easily
    (global-set-key     (kbd "C-x x")   'pop-local-mark)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-02-select-copy)

;;; 01-function-02-select-copy.el ends here
