;;; shortcut-global.el --- a config file for global Emacs shortcut

;; Copyright (c) 2006-2012 Claude Tete
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

;; Keywords: config, shortcut, emacs
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.7
;; Created: October 2006
;; Last-Updated: October 2012

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-global'
;;              var     `section-shortcut'

;;; Change Log:
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
(global-set-key         [f4]                    'kill-buffer-and-window)

;; run ispell (dictionary) (set language in `section-misc')
;; never used
(global-set-key         (kbd "<f7>")            'ispell-buffer)
(global-set-key         (kbd "<S-f7>")          'ispell-word)
(global-set-key         (kbd "<M-f7>")          'ispell-region)
(global-set-key         (kbd "<C-f7>")          'ispell-comments-and-strings)
(global-set-key         (kbd "<C-M-f7>")        'ispell-change-dictionary)

;;;; insert printf or ifdef for debug (used in epita kaneton project)
;;(global-set-key         [f7]                    'printf-debug-shortcut)
;;(global-set-key         [f8]                    'ifdef-debug-shortcut)

;;;; run apropos for the word at point
;;(global-set-key [f1]                            'vectra-apropos-on-word)

;;;; search a file in the current folder (recursively)
;;(global-set-key         "\M-g"                  'find-grep-dired)

;; go to line #
(global-set-key         "\M-g"                  'goto-line)
(global-set-key         "\M-L"                  'what-line)

;; replace string
(global-set-key         "\M-r"                  'replace-string)

;; save current buffer as a bookmark
(global-set-key         "\C-cv"                 'bookmark-set)

;;;; ??
;;(global-set-key         "\C-up"                 'backward-sentence)

;; delete backward a word
(global-set-key         "\C-z"                  'backward-kill-word)

;; copy line in kill-ring `C-j' (no more C-k C-y)
(global-set-key         "\C-j"                  'push-line)
;; add a new line whitout jump on it
(global-set-key         (kbd "<S-return>")      'open-line)
;; join next line with the current and remove space between except one
(global-set-key         (kbd "<H-return>")      '(lambda ()
                                                   (interactive)
                                                   (move-end-of-line nil)
                                                   (next-line)
                                                   (join-line)
                                                   (just-one-space)
                                                   ))
;; new line but from anywhere on the previous line
(global-set-key         (kbd "<M-return>")      '(lambda ()
                                                   (interactive)
                                                   (end-of-line)
                                                   (newline)
                                                   ))

;; new line but from anywhere on the next line
(global-set-key         (kbd "<C-M-return>")    '(lambda ()
                                                   (interactive)
                                                   (move-beginning-of-line nil)
                                                   (open-line 1)
                                                   ))

;; align a region following regexp
(global-set-key         "\C-cpp"                'align)
(global-set-key         "\C-cpl"                'align-regexp)

;; remove a pair of matched parentheses/brackets
;; see "M-(" to create a pair of matched parentheses around the region
(global-set-key         (kbd "C-(")             'delete-pair)

;; refresh current buffer
(global-set-key         (kbd "M-p")             'revert-buffer)

;; switch between header/source file
(global-set-key         (kbd "C-`")             'ff-find-related-file)

;; switch between others header/source files
(global-set-key         [\C-f4]                 'ff-find-other-file)

;; incremental search at point
(global-set-key         (kbd "C-M-x")           'isearch-forward-at-point)

;; fill paragraph at point
(global-set-key         (kbd "C-c ]")           'fill-paragraph)
;; fill region
(global-set-key         (kbd "C-c [")           'fill-region)

;; change local dictionary
(global-set-key         (kbd "C-c $")           'ispell-change-dictionary)

;; (un)comment region
(global-set-key         (kbd "H-/")             'comment-or-uncomment-region)

;; move to the matched parenthesis
(global-set-key         (kbd "<H-right>")       'forward-sexp)
(global-set-key         (kbd "<H-left>")        'backward-sexp)

;; run calc quick
(global-set-key         (kbd "<M-kp-multiply>") 'quick-calc)

;; run hippie expand
(global-set-key         (kbd "M-?")             'hippie-expand)

;; run rss reader
(global-set-key         (kbd "H-r")             'newsticker-show-news)

;; insert register
(global-set-key         (kbd "C-c i")           'insert-register)

;; (by Fabrice Niessen)
;; It's more or less a convention that each language mode binds its symbol
;; completion command to `M-TAB' which is a reserved hot key under Windows.
;; Way to solve this: when you hit `C-TAB', the command normally bound to
;; `M-TAB' will be called.
(global-set-key         (kbd "<C-tab>")
  '(lambda ()
     (interactive)
     (call-interactively (key-binding (kbd "M-TAB")))
     )
  )

;;
;;; HOME/END
;; REQUIREMENT: var     `section-external-home-end'
(when section-mode-home-end
  ;; bind home with new features
  (global-set-key       (kbd "<home>")          'pc-keys-home)
  ;;
  ;; bind home with new features
  (global-set-key       (kbd "<end>")           'pc-keys-end)
  )

;;
;;; OUTLINE
;; REQUIREMENT: var     `section-mode-outline'
(when section-mode-outline
  ;; bind toggle hide/show block
  (global-set-key       (kbd "C-c h")           'outline-toggle-children)
  )

;;
;;; DIRED PLUS
;; REQUIREMENT: var     `section-mode-dired-plus'
(when section-mode-dired-plus
  (eval-after-load "dired"
    '(progn
       ;; open with default associated application
       (define-key dired-mode-map (kbd "<H-return>") 'dired-w32-browser)
       (define-key dired-mode-map (kbd "[")          'dired-up-directory)
       )
    )
  )

;;
;;; EDIFF
(eval-after-load "ediff"
  '(progn
     ;; I think it does not not work
     ;; previous diff
     (local-set-key     (kbd "<M-up>")          'ediff-previous-difference)
     ;; next diff
     (local-set-key     (kbd "<M-down>")        'ediff-next-difference)
     ;; get modification from left
     (local-set-key     (kbd "<M-right>")       'ediff-copy-A-to-B)
     ;; get modification from right
     (local-set-key     (kbd "<M-left>")        'ediff-copy-B-to-A)
     )
  )

;;
;;; BROWSE KILL RING
(when section-mode-browse-kill-ring
  (eval-after-load "browse-kill-ring"
    '(progn
       ;; move next and previous with arrow
       (define-key browse-kill-ring-mode-map (kbd "<up>")   'browse-kill-ring-previous)
       (define-key browse-kill-ring-mode-map (kbd "<down>") 'browse-kill-ring-forward)
       ;; quit not only with q but also with C-g
       (define-key browse-kill-ring-mode-map (kbd "C-g")    'browse-kill-ring-quit)
       )
    )
  )

;;
;;; FOLD DWIM
(when section-mode-fold-dwim
  ;; show/hide block
  (global-set-key       (kbd "<M-left>")        '(lambda ()
                                                   (interactive)
                                                   (outline-up-heading 1)
                                                   (fold-dwim-toggle)))
  (global-set-key       (kbd "<M-right>")       '(lambda ()
                                                   (interactive)
                                                   (outline-up-heading 1)
                                                   (fold-dwim-toggle)))
  ;; hide all
  (global-set-key       (kbd "<M-up>")          'fold-dwim-hide-all)
  ;; show all
  (global-set-key       (kbd "<M-down>")        'fold-dwim-show-all)
  )

;;
;;; HIDE SHOW
(when section-languages-c-hide-show
  (add-hook 'outline-minor-mode-hook
    '(lambda ()
       ;; hide all
       (local-set-key   (kbd "C-,")             '(lambda ()
                                                   (interactive)
                                                   (hide-sublevels 1)))
       ;; show all
       (local-set-key   (kbd "C-.")             'show-all)
       ;; hide current block
       (local-set-key   (kbd "<M-left>")        '(lambda ()
                                                   (interactive)
                                                   (outline-up-heading 1)
                                                   (hide-subtree)))
       ;; show block
       (local-set-key   (kbd "<M-right>")       'show-subtree)
       )
    )
  )

;;
;;; ALIAS
;; replace with regex
(defalias 'rr 'replace-regexp)
;; eval elisp buffer
(defalias 'eb 'eval-buffer)
;; eval elisp region
(defalias 'er 'eval-region)
;; insert date (format YYYY-MM-DD)
(defalias 'id 'insert-date)


;;
;;; CUA
;; not used
(when section-shortcut-global-cua (message "    8.1.1 CUA Shortcut...")
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ; No region when it is not highlighted
  (setq cua-keep-region-after-copy t) ; Standard Windows behaviour
  (message "    8.1.1 CUA Shortcut... Done"))


(provide 'shortcut-global)

;;; shortcut-global.el ends here
