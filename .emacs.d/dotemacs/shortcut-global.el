;;; shortcut-global.el --- a config file for global Emacs shortcut

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

;; Keywords: config, shortcut, emacs
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 4.0
;; Created: October 2006
;; Last-Updated: September 2016

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-global'
;;              var     `section-shortcut'

;;; Change Log:
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
(global-set-key         (kbd "<f9>")              'tag-insert-shortcut)

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
                                                   (just-one-space 0)
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
(global-set-key         (kbd "C-c a a")         'align)
(global-set-key         (kbd "C-c a s")         'align-regexp)

;; remove a pair of matched parentheses/brackets
;; see "M-(" to create a pair of matched parentheses around the region
(global-set-key         (kbd "C-(")             'delete-pair)

;; refresh current buffer
(global-set-key         (kbd "M-p")             'revert-buffer)

;; switch between header/source file
(global-set-key         (kbd "C-`")             'ff-find-related-file)

;; switch between others header/source files
(global-set-key         [\C-f4]                 'ff-find-other-file)

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

;;
;;; EXPAND
;; run hippie expand
(global-set-key         (kbd "M-?")             'hippie-expand)
;; company mode
(when section-mode-company
  (if section-mode-helm
    (eval-after-load 'company
      '(progn
         ;; use helm with comapny mode
         (define-key company-mode-map     (kbd "C-/")     'helm-company)
         (define-key company-active-map   (kbd "C-/")     'helm-company)))
    (global-set-key     (kbd "C-/")             'company-complete)))

;; run rss reader
(global-set-key         (kbd "H-r")             'newsticker-show-news)

;; insert register
(global-set-key         (kbd "C-c i")           'insert-register)

;; kill forward a word (like M-d)
(global-set-key         (kbd "<M-delete>")      'kill-word)
;; kill forward a line (like C-k)
(global-set-key         (kbd "<C-delete>")      'kill-line)
;; kill backward a line (like C-u C-k)
(global-set-key         (kbd "<C-backspace>")   '(lambda ()
                                                   (interactive)
                                                   (kill-line -1)))

;; cua rectangle
(eval-after-load "cua-rect"
  '(progn
     (define-key cua--rectangle-keymap [remap kill-ring-save-x] 'cua-copy-region)
     )
  )

;;
;;; POPWIN
;; quit helm with F2 or resume his last session
(when section-mode-popwin
  (global-set-key               (kbd "<f2>")            '(lambda ()
                                                           (interactive)
                                                           (if (popwin:popup-window-live-p)
                                                             (popwin:close-popup-window)
                                                             ;;FIXME check if helm buffer to use helm-resume
                                                             (popwin:popup-last-buffer))))
  )

;;
;;; HELM
(when section-mode-helm
  (with-eval-after-load "helm"
    ;; quit helm with ESCAPE
    (define-key helm-map        (kbd "<escape>")        'helm-keyboard-quit)
    ;; quit helm with f2
    (define-key helm-map        (kbd "<f2>")            'helm-keyboard-quit)
    ;; go to beginning of buffer
    (define-key helm-map        (kbd "M-<home>")        'helm-beginning-of-buffer)
    (define-key helm-map        (kbd "C-<home>")        'helm-beginning-of-buffer)
    ;; go to end of buffer
    (define-key helm-map        (kbd "M-<end>")         'helm-end-of-buffer)
    (define-key helm-map        (kbd "C-<end>")         'helm-end-of-buffer)
    ;; show previous element in history
    (define-key helm-map        (kbd "M-<up>")          'previous-history-element)
    ;; show next element in history
    (define-key helm-map        (kbd "M-<down>")        'next-history-element)

    ;; to replace yank-pop by helm-kill-ring
    (when section-mode-helm-kill-ring
      (global-set-key           (kbd "M-y")             '(lambda ()
                                                           (interactive)
                                                           (if (eq last-command 'yank)
                                                             (progn
                                                               (setq last-command 'yank)
                                                               (yank-pop))
                                                             (helm-show-kill-ring)))))
    ;; to replace M-x by helm
    (when section-mode-helm-M-x
      (global-set-key           (kbd "M-x")             'helm-M-x))
    ;; to replace buffer list same shortcut than with ECB
    (when section-mode-helm-buffers-list
      (global-set-key           (kbd "M-z")             'helm-buffers-list))
    ;; to have imenu in helm with function/variable list

    ;; add helm menu shortcut
  (when section-mode-helm-imenu
    (global-set-key             (kbd "M-\\")             'helm-semantic-or-imenu))
    ) ; (with-eval-after-load "helm"
  ) ; (when section-mode-helm

;;
;;; C
;; remove report bug bind in c-mode to bind helm-bookmark
(when section-mode-c
  (add-hook 'c-mode-common-hook '(lambda () (local-unset-key (kbd "C-c C-b")))))

;;
;;; CEDET
;; to compile
(when section-mode-cedet-ecb
  (add-hook 'compilation-filter-hook '(lambda ()
                                      (ecb-goto-window-compilation)
                                      (end-of-buffer)))
  ) ; (when section-mode-cedet-ecb
(global-set-key       (kbd "<f10>")           'compile)

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

;; bind zap-to-char because I use M-z with ECB
(global-set-key         (kbd "H-z")             'zap-to-char)

;; delete all blank character or line except one
(global-set-key         (kbd "<M-SPC>")         'just-one-space-or-line)

;; move page to page (to next/previous character)
(global-set-key         (kbd "<M-up>")          'backward-page)
(global-set-key         (kbd "<M-down>")        'forward-page)

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
;;; MAGIT
(when section-mode-magit
  ;; git status (entry point of magit)
  (global-set-key       (kbd "C-c g g")           'magit-status)
  ;; git pull
  (global-set-key       (kbd "C-c g p")           'magit-pull)
  ;; git log
  (global-set-key       (kbd "C-c g l")           'magit-log)
  ;; git standard output
  (global-set-key       (kbd "C-c g d")           '(lambda ()
                                                     (interactive)
                                                     (magit-display-process)
                                                     (switch-to-buffer "*magit-process*")))
  )

;;
;;; ELPY
(when section-mode-elpy
  (add-hook 'elpy-mode-hook
    (lambda ()
      (local-unset-key (kbd "M-TAB"))
      (define-key elpy-mode-map (kbd "<M-up>")          'backward-page)
      (define-key elpy-mode-map (kbd "<M-down>")        'forward-page)
      ))
  )

;;
;;; DIRED PLUS
;; REQUIREMENT: var     `section-mode-dired-plus'
(when section-mode-dired-plus
  (eval-after-load "dired"
    '(progn
       ;; open with default associated application
       (define-key dired-mode-map       (kbd "<H-return>")      'dired-w32-browser)
       (define-key dired-mode-map       (kbd "[")               'dired-up-directory)
       )
    )
  )

;;
;;; EDIFF
(when section-mode-ediff
  (add-hook 'ediff-startup-hook
    '(lambda ()
       ;; previous diff
       (define-key ediff-mode-map       (kbd "<M-up>")          'ediff-previous-difference)
       ;; next diff
       (define-key ediff-mode-map       (kbd "<M-down>")        'ediff-next-difference)
       ;; get modification from left
       (define-key ediff-mode-map       (kbd "<M-right>")       'ediff-copy-A-to-B)
       ;; get modification from right
       (define-key ediff-mode-map       (kbd "<M-left>")        'ediff-copy-B-to-A)
       )
    )
  )

;;
;;; BROWSE KILL RING
(when section-mode-browse-kill-ring
  (add-hook 'browse-kill-ring-hook
    '(lambda ()
       ;; move next and previous with arrow
       (local-set-key   (kbd "<up>")            'browse-kill-ring-previous)
       (local-set-key   (kbd "<down>")          'browse-kill-ring-forward)
       ;; quit not only with q but also with C-g or F2 with ecb
       (local-set-key   (kbd "C-g")             'browse-kill-ring-quit)
       (when section-mode-cedet-ecb
         (local-set-key (kbd "<f2>")            'browse-kill-ring-quit))
       (local-set-key   (kbd "<escape>")        'browse-kill-ring-quit)
       (local-set-key   (kbd "<return>")        'browse-kill-ring-insert-move-and-quit)
       )))

;;
;;; FOLD DWIM
(when section-mode-fold-dwim
  (add-hook 'c-mode-hook
    '(lambda ()
       ;; show/hide block
       (local-set-key   (kbd "<M-left>")        'fold-dwim-toggle)
       (local-set-key   (kbd "<M-right>")       'fold-dwim-toggle)
       ;; hide all
       (local-set-key   (kbd "<M-up>")          'fold-dwim-hide-all)
       ;; show all
       (local-set-key   (kbd "<M-down>")        'fold-dwim-show-all)
       ))
  ;; only when rtrt is used
  (when section-mode-rtrt-script
    (add-hook 'rtrt-script-mode-hook
      '(lambda ()
         ;; hide all
         (local-set-key         (kbd "<H-prior>")       'fold-dwim-hide-all)
         ;; show all
         (local-set-key         (kbd "<H-next>")        'fold-dwim-show-all)
         )))
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
;;       (local-set-key   (kbd "<M-left>")        '(lambda ()
;;                                                   (interactive)
;;                                                   (outline-up-heading 1)
;;                                                   (hide-subtree)))
;;       ;; show block
;;       (local-set-key   (kbd "<M-right>")       'show-subtree)
       )))

;;
;;; UNDO TREE
(when section-mode-undo-tree
  (global-set-key       (kbd "C-_")             'undo-tree-undo)
  (global-set-key       (kbd "M-_")             'undo-tree-redo)
  (global-set-key       (kbd "C-M-_")           'undo-tree-visualize)
  (add-hook 'undo-tree-visualizer-mode-hook
    (lambda ()
      ;; enter and 'q' key will quit undo tree
      (local-set-key    (kbd "<return>")        '(lambda ()
                                                   (interactive)
                                                   (undo-tree-visualizer-quit)
                                                   (when section-mode-cedet-ecb
                                                     (ecb-toggle-compile))
                                                   ))
      (local-set-key    (kbd "q")               '(lambda ()
                                                   (interactive)
                                                   (undo-tree-visualizer-quit)
                                                   (when section-mode-cedet-ecb
                                                     (ecb-toggle-compile))
                                                   ))
      ;; page up/down will undo/redo 10 times
      (local-set-key    (kbd "<prior>")         '(lambda ()
                                                   (interactive)
                                                   (cl-loop repeat 10 do
                                                     (undo-tree-visualize-undo))))
      (local-set-key    (kbd "<next>")          '(lambda ()
                                                   (interactive)
                                                   (cl-loop repeat 10 do
                                                     (undo-tree-visualize-redo))))
      ;; inverse C-up/down
      (local-set-key    (kbd "<C-up>")          'undo-tree-visualize-undo-to-x)
      (local-set-key    (kbd "<C-down>")        'undo-tree-visualize-redo-to-x)
      )))

;;
;;; EXPAND-REGION
(when section-mode-expand-region
  ;; M-s then s, s, etc to expand selection region
  (global-set-key       (kbd "M-s")             'er/expand-region)
  )

;;
;;; SYNERGY
(when section-mode-synergy
  ;; start
  (global-set-key       (kbd "C-c s s")         'synergy-start)
  ;; checkout
  (global-set-key       (kbd "C-c s c")         'synergy-checkout)
  ;; checkout with current task
  (global-set-key       (kbd "C-c s x")         'synergy-checkout-with-current-task)
  ;; uncheckout
  (global-set-key       (kbd "C-c s d")         'synergy-uncheckout)
  ;; diff
  (global-set-key       (kbd "C-c s =")         'synergy-diff-with-previous)
  ;; history
  (global-set-key       (kbd "C-c s h")         'synergy-history)
  ;; reconcile to server
  (global-set-key       (kbd "C-c s r")         'synergy-reconcile-to-server)
  ;; reconcile from server
  (global-set-key       (kbd "C-c s w")         'synergy-reconcile-to-local)
  ;; show conflict
  (global-set-key       (kbd "C-c s f")         'synergy-show-conflict)
  ;; create task
  (global-set-key       (kbd "C-c s t c")       'synergy-create-task)
  ;; set current task
  (global-set-key       (kbd "C-c s t d")       'synergy-set-current-task)
  ;; set task release
  (global-set-key       (kbd "C-c s t r")       'synergy-set-task-as-current)
  ;; complete/checkin task
  (global-set-key       (kbd "C-c s t i")       'synergy-checkin-task)
  ;; update/reconfigure
  (global-set-key       (kbd "C-c s u")         'synergy-reconfigure)
  ;; update/reconfigure current
  (global-set-key       (kbd "C-c s p")         'synergy-reconfigure-current-project)
  ) ; (when section-function-synergy


;; yank menu in popup
(global-set-key         (kbd "C-M-y")           '(lambda ()
                                                   (interactive)
                                                   (popup-menu 'yank-menu)))
;;
;;; HIDE-LINES
(when section-mode-hide-lines
  ;; hide match
  (global-set-key       (kbd "C-c h h")         'hide-lines-matching)
  ;; hide not match
  (global-set-key       (kbd "C-c h n")         'hide-lines-not-matching)
  ;; show all
  (global-set-key       (kbd "C-c h s")         'hide-lines-show-all)
  ) ; (when section-mode-hide-lines

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
(when section-mode-cedet-ecb
  (defalias 'prr 'ps2pdf-from-region)
  (defalias 'prb 'ps2pdf-from-buffer)
  )

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
