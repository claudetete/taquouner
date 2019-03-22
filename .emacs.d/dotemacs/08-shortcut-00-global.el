;;; 08-shortcut-00-global.el --- a config file for global Emacs shortcut

;; Copyright (c) 2006-2019 Claude Tete
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
;; Version: 4.3
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;; section comment
;; [HEADER.custom binding or shortcut for everything in GNU Emacs (thought for qwerty keyboard)]
;; [DEFAULT.t]
;;
;; subsection comment
;; [SUBHEADER.add global shortcut (for whole Emacs)]
;; [SUBDEFAULT.t]


;;; Code:
;;;; if C-h do not do backspaces
;;(normal-erase-is-backspace-mode)

;; close current buffer and its window
(global-set-key         (kbd "<f4>")            'kill-buffer-and-window)

;; go to line #
(global-set-key         (kbd "M-g")             'goto-line)
(global-set-key         (kbd "M-L")             'what-line)

;; replace string
(global-set-key         (kbd "M-r")             'replace-string)

;; save current buffer as a bookmark
(global-set-key         (kbd "C-c v")           'bookmark-set)

;; delete backward a word
(global-set-key         (kbd "C-z")             'backward-kill-word)

;; add a new line whitout jump on it
(global-set-key         (kbd "<S-return>")      'open-line)
;; join next line with the current and remove space between except one
(global-set-key         (kbd "<H-return>")      (lambda ()
                                                  (interactive)
                                                  (move-end-of-line nil)
                                                  (next-line)
                                                  (join-line)
                                                  (just-one-space 0)
                                                  ))
;; new line but from anywhere on the previous line
(global-set-key         (kbd "<M-return>")      (lambda ()
                                                  (interactive)
                                                  (end-of-line)
                                                  (newline)
                                                  ))

;; new line but from anywhere on the next line
(global-set-key         (kbd "<C-M-return>")    (lambda ()
                                                  (interactive)
                                                  (move-beginning-of-line nil)
                                                  (open-line 1)
                                                  ))

;; align a region following regexp
(global-set-key         (kbd "C-c a a")         'align)
(global-set-key         (kbd "C-c a s")         'align-regexp)

;; refresh current buffer
(global-set-key         (kbd "M-p")             'revert-buffer)
(global-set-key         (kbd "<f5>")            'revert-buffer)

;; switch between header/source file
(global-set-key         (kbd "C-`")             'ff-find-other-file)

;; switch between others header/source files
(global-set-key         (kbd "<C-f4>")          'ff-find-other-file)

;; fill paragraph at point
(global-set-key         (kbd "C-c ]")           'fill-paragraph)
;; fill region
(global-set-key         (kbd "C-c [")           'fill-region)
(global-set-key         (kbd "H-]")             'fill-region-as-paragraph)

;; run calc quick
(global-set-key         (kbd "<M-kp-multiply>") 'quick-calc)

;;
;;; EXPAND
;; run hippie expand
(global-set-key         (kbd "M-?")             'hippie-expand)

;; run rss reader
(global-set-key         (kbd "H-r")             'newsticker-show-news)

;; insert register
(global-set-key         (kbd "C-c i")           'insert-register)

;; kill backward a line (like C-u C-k)
(global-set-key         (kbd "<C-backspace>")   (lambda ()
                                                  (interactive)
                                                  (kill-line -1)))

;; cua rectangle, kill will do cua copy
(eval-after-load "cua-rect"
  '(progn
     (define-key cua--rectangle-keymap [remap kill-ring-save-x] 'cua-copy-region)
     )
  )

;;
;;; RECTANGLE
(when (or tqnr-running-on-emacs-24-4 tqnr-running-on-emacs-24-5 tqnr-running-on-emacs-25)
  (when (not tqnr-section-mode-hydra)
    (global-set-key       (kbd "<C-return>")      'rectangle-mark-mode))
  (with-eval-after-load "rect"
    ;; insert string (interactively, Enter to apply)
    (define-key rectangle-mark-mode-map         (kbd "t")               'string-rectangle)
    (define-key rectangle-mark-mode-map         (kbd "s")               'string-rectangle)
    (define-key rectangle-mark-mode-map         (kbd "i")               'string-rectangle)
    ;; useful to to enter in rectangle mode and insert with C-return twice
    (define-key rectangle-mark-mode-map         (kbd "C-RET")           'string-rectangle)
    ;; insert rectangle of space (C-x r o and C-o)
    (define-key rectangle-mark-mode-map         (kbd "SPC")             'open-rectangle)

    ;; Kill rectangle (C-x r k)
    (define-key rectangle-mark-mode-map         (kbd "C-w")             'kill-rectangle)
    ;; Copy rectangle (C-x r M-w)
    (define-key rectangle-mark-mode-map         (kbd "M-w")             'copy-rectangle-as-kill)
    ;; Delete rectangle (delete key also works) (C-x r d)
    (define-key rectangle-mark-mode-map         (kbd "d")               'delete-rectangle)
    ;; Yank rectangle (C-x r y)
    (define-key rectangle-mark-mode-map         (kbd "C-y")             'yank-rectangle)
    ;; Insert incremented number (C-x r N)
    (define-key rectangle-mark-mode-map         (kbd "n")               'rectangle-number-lines-wrapper)
    ;; Clear rectangle, insert space instead of rectangle (C-x r c)
    (define-key rectangle-mark-mode-map         (kbd "S-SPC")           'clear-rectangle)
    ;; Trim rectangle, remove space from left
    (define-key rectangle-mark-mode-map         (kbd "M-SPC")           'delete-whitespace-rectangle)
    ;; move from corner to corner
    (define-key rectangle-mark-mode-map         (kbd "RET")             'rectangle-exchange-point-and-mark)
    )
  )

;; to compile
(global-set-key       (kbd "<f10>")           'compile)

;; (by Fabrice Niessen)
;; It's more or less a convention that each language mode binds its symbol
;; completion command to `M-TAB' which is a reserved hot key under Windows.
;; Way to solve this: when you hit `C-TAB', the command normally bound to
;; `M-TAB' will be called.
(global-set-key         (kbd "C-TAB")
  (lambda ()
    (interactive)
    (call-interactively (key-binding (kbd "M-TAB")))))

;; bind zap-to-char because I use M-z
(global-set-key         (kbd "H-z")             'zap-to-char)

;; move page to page (to next/previous character)
(global-set-key         (kbd "<C-M-prior>")     'backward-page)
(global-set-key         (kbd "<C-M-next>")      'forward-page)

;; yank menu in popup
(global-set-key         (kbd "C-M-y")           (lambda ()
                                                  (interactive)
                                                  (popup-menu 'yank-menu)))

;;
;;; ALIAS
;; replace with regex
(defalias 'rr 'replace-regexp)
;; eval elisp buffer
(defalias 'eb 'eval-buffer)
;; eval elisp region
(defalias 'er 'eval-region)


;;
;; [[VARCOMMENT.CUA: enable C-x, C-c, C-v to cut copy paste
;; don't recommend it otherwise see http://www.emacswiki.org/CuaMode
;; ]]
;; [VARIABLE.tqnr-section-shortcut-global-cua nil]
(when tqnr-section-shortcut-global-cua (message "      CUA Shortcut...")
  (cua-mode t)
  ;; Don't tabify after rectangle commands
  (setq cua-auto-tabify-rectangles nil)
  ;; No region when it is not highlighted
  (transient-mark-mode 1)
  ;; Standard Windows behaviour
  (setq cua-keep-region-after-copy t)
  (message "      CUA Shortcut... Done"))


(provide '08-shortcut-00-global)

;;; 08-shortcut-00-global.el ends here
