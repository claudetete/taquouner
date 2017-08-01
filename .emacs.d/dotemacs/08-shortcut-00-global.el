;;; 08-shortcut-00-global.el --- a config file for global Emacs shortcut

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
;; Version: 4.2
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;; section comment
;; [HEADER.custom binding or shortcut for everything in GNU Emacs (thought for qwerty keyboard)]
;;
;; subsection comment
;; [SUBHEADER.add global shortcut (for whole Emacs)]

;;; Change Log:
;; 2017-07-25 (4.2)
;;    update to new conf format
;; 2017-05-26 (4.1)
;;    clean up + remove essential hyper shortcut (not on all keyboard...) +
;;    use rectangle mode and no more cua rectangle + modify shortcut to improve
;;    org mode compatibility + add haskell shortcut
;; 2016-09-28 (4.0)
;;    modify align shortcut (helm/projectile conflict) + add helm/company
;;    shortcuts + add popwin shortcut + add helm shortcut + remove bind into c
;;    mode (helm conflict) + add elpy shortcut + undo tree shortcut
;; 2015-08-21 (3.9)
;;    add condition to cua-rect shortcut + add hide-lines shortcuts
;; 2013-09-10 (3.8)
;;    add magit shortcut
;; 2013-05-30 (3.7)
;;    fix bug with shortcut in ediff mode
;; 2013-05-23 (3.6)
;;    remove mixtab shortcut
;; 2013-05-07 (3.5)
;;    remove helm shortcut to close compile window
;; 2013-04-24 (3.4)
;;    do not use tabmix with rtrt (disable indentation in rtrt mode)
;; 2013-04-11 (3.3)
;;    shorcut for just-one-space-or-line + C-y M-y will not call helm-kill-ring
;; 2013-04-08 (3.2)
;;    add shortcut for zap to char function (default bind is used by ECB bind) +
;;    helm mode + fix bug with ediff + more comfort for browse kill ring and
;;    undo tree + fold dwim depend of major mode
;; 2012-12-27 (3.1)
;;    add alias for ps2pdf to print with color
;; 2012-12-04 (3.0)
;;    add compile shortcut (from shortcut-grep.el)
;; 2012-11-29 (2.9)
;;    wrong shortcut for copy rectangle in cua rectangle mode
;; 2012-10-31 (2.8)
;;    have logical shortcuts with backspace and delete
;; 2012-10-26 (2.7)
;;    add shortcut for insert register, fold or hide show + alias insert date
;; 2012-10-19 (2.6)
;;    remove home/end old shortcuts
;; 2012-08-01 (2.5)
;;    add shortcut to change dictionnary with ispell + move shortcut
;; 2012-07-19 (2.4)
;;    new shortcut for browse kill ring + add fold dwim
;; 2012-07-09 (2.3)
;;    new line from anywhere + hippie expand + rss reader + browse kill ring +
;;    new aliases
;; 2012-06-26 (2.2)
;;    new line from anywhere on the line + folder up in dired
;; 2012-06-21 (2.1)
;;    join-line without space and with next line + quick calc + add condition to
;;    open with in dired mode + comments about ediff + alias for replace-regexp
;; 2012-06-19 (2.0)
;;    enable shortcuts for ispell with F7
;; 2012-06-08 (1.9)
;;    add shortcut to (un)comment, and move by parenthesis
;; 2012-05-29 (1.8)
;;    add shortcuts to fill region and to change dictionary in ispell mode
;; 2012-05-10 (1.7)
;;    change goto line shortcut to not override the default downcase-word +
;;    shortcut for dired plus to avoid problem with CUA selection
;; 2012-05-03 (1.6)
;;    add shortcut to add line without jump + give number of line + bind C-TAB
;;    to M-TAB + remove shortcut to calendar
;; 2012-04-20 (1.5)
;;    add CUA mode
;; 2012-04-03 (1.4)
;;    add shortcut for fill-paragraph
;; 2012-03-30 (1.3)
;;    translate comments in English
;; 2011-08-03 (1.2)
;;    add shortcut for pair of parentheses
;; 2011-07-27 (1.1)
;;    change bind of C-cpp to align
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


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

;; switch between header/source file
(global-set-key         (kbd "C-`")             'ff-find-other-file)

;; switch between others header/source files
(global-set-key         (kbd "<C-f4>")          'ff-find-other-file)

;; fill paragraph at point
(global-set-key         (kbd "C-c ]")           'fill-paragraph)
;; fill region
(global-set-key         (kbd "C-c [")           'fill-region)

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
  (global-set-key       (kbd "<C-return>")      'rectangle-mark-mode)
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
    (define-key rectangle-mark-mode-map         (kbd "C-k")             'kill-rectangle)
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
