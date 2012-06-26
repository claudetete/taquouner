;;; functions.el --- a config file to add some function

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

;; Keywords: config, function
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 4.5
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-external-functions'
;;
;; it need to be split...

;;; Change Log:
;; 2012-06-26 (4.5)
;;    fix bug with mark about web search + add google
;; 2012-06-21 (4.4)
;;    add function to open web browser with text selected in Emacs + insert date
;;    + split ecb and clearcase function in new file +
;; 2012-06-14 (4.3)
;;    clean up
;; 2012-06-12 (4.2)
;;    change slick copy to more compatibility + add condition to eval some
;;    functions + remove hide/show function
;; 2012-06-08 (4.1)
;;    add slick copy (copy when not selected) + (un)comment + scroll without
;;    moving cursor + maximize function + some functions to test
;; 2012-06-05 (4.0)
;;    start to split + remove dead source code
;; 2012-05-29 (3.9)
;;    add function for integration with clearcase
;; 2012-05-14 (3.8)
;;    add function to improve tab key when hide show mode
;; 2012-05-03 (3.7)
;;    add function to checkout/diff/history file from clearcase + remove hippie
;;    expand custom
;; 2012-05-02 (3.6)
;;    add function about isearch, macro, windows swap + comment for fix
;;    fullscreen bug
;; 2012-03-29 (3.5)
;;    add function align with =
;; 2012-03-28 (3.4)
;;    translate comments in English and change old format
;; 2012-03-26 (3.3)
;;    fix bug with rtrt align
;; 2012-03-20 (3.2)
;;    add function for rtrt align and replace
;; 2012-03-12 (3.1)
;;    add conditions for each working environment
;; 2012-03-02 (3.0)
;;    add function occur-at-point
;; 2011-11-03 (2.9)
;;    add function to expand C macro per project
;; 2011-10-27 (2.8)
;;    change grep function per project to add list file
;; 2011-03-10 (2.7)
;;    add gtags function
;; 2010-12-02 (2.6)
;;    add dos2unix and unix2dos
;; 2010-11-19 (2.5)
;;    align regexp
;; 2010-11-03 (2.4)
;;    push line + search fault and tab
;; 2010-09-02 (2.3)
;;    insert tab + config
;; 2010-08-11 (2.2)
;;    select word + startup
;; 2010-07-09 (2.1)
;;    ecb
;; 2010-06-11 (2.0)
;;    grep
;; 2010-06-09 (1.6)
;;    etags
;; 2008-04-21 (1.5)
;;    insert header
;; 2008-03-10 (1.0)
;;    add printf for debug /* Kaneton :) */
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;;;  CONST
(defconst clt-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")

;;
;;;
;;;; FLAG DEBUG
;;; insert printf for debug (by Kevin Prigent)
;;   printf("\n"); //DEBUG
(defun printf-debug-shortcut ()
  "Insert a printf(\"n\"); //DEBUG."
  (interactive)
  (insert "printf(\"\\n\");//DEBUG")
  (indent-according-to-mode) (backward-char 12)
  )
;;; insert a test of macro for debug (by Kevin Prigent)
;;   #ifdef OUR_DEBUG
;;     \n
;;   #endif
(defun ifdef-debug-shortcut ()
  "Insert a #ifdef OUR_DEBUG."
  (interactive)
  (insert "#ifdef OUR_DEBUG\n"
      "\t\n"
      "#endif\n")
  (indent-according-to-mode) (backward-char 8)
  )

;;
;;;
;;;; SELECT WORD/REGION
;;; match a string (by Claude TETE)
(defun clt-match-string (n)
  "Match a string (N)."
  (buffer-substring (match-beginning n) (match-end n)))
;;; select the word at point (by Claude TETE)
(defun clt-select-word ()
  "Select the word under cursor."
  (cond
    ((looking-at "[0-9A-Za-z_]")
      (while (and (not (bolp)) (looking-at "[0-9A-Za-z_]"))
        (forward-char -1))
      (if (not (looking-at "[0-9A-Za-z_]")) (forward-char 1)))
    (t
      (while (looking-at "[ \t]")
        (forward-char 1))))
  (if (looking-at clt-symbol-regexp)
    (clt-match-string 0) nil)
  )
;;; interactive select word at point (by Claude TETE)
(defun select-word-under ()
  "Select the word under cursor."
  (interactive)
  (let (pt)
    (when (boundp 'auto-highlight-symbol-mode)
      (ahs-clear))
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)
    )
  )
;;; get the string from word at point or region
(defun clt-get-string-position ()
  (let ()
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (list (car (bounds-of-thing-at-point 'word)) (cdr (bounds-of-thing-at-point 'word)))
      )
    )
  )

;;
;;;
;;;; COPY/CUT
;; Change cutting behavior (from http://emacswiki.org/emacs/WholeLineOrRegion):
(put 'kill-ring-save 'interactive-form
  '(interactive
     (if (use-region-p)
       (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))
(put 'kill-region 'interactive-form
  '(interactive
     (if (use-region-p)
       (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))

;;
;;;
;;;; SEARCH
;;; search the word at the point for the whole buffer (by Claude TETE
(defun occur-word-at-point ()
  "Search the word under cursor in the current buffer."
  (interactive)
  (let (word prompt input)
    (setq word (clt-select-word))
    (if word
      (setq prompt (concat "List lines matching regexp (default " word "): "))
      (setq prompt "List lines matching regexp: "))
    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))
    (occur word)
    )
  )
;;
;; occur when incremental search (by Fabrice Niessen)
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
;;
;;; incremental search the word at the point (from www.emacswiki.org)
;; I-search with initial contents
(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  "Set initialization of isearch."
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update)
  )
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point (optional REGEXP-P and NO-RECURSIVE-EDIT)."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
            (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
        (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)
        )
      )
    )
  )

;;
;;;
;;;; MACRO
;; toggle macro recording on/off (by Fabrice Niessen)
(defun my-toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))
;; toggle macro recording on/off (by Fabrice Niessen)
(defun my-toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)
  (end-kbd-macro))
;; shortcuts are put in shortcut-function.el

;;
;;;
;;;; SEMANTIC
(when section-mode-cedet-semantic
 (defvar semantic-tags-location-ring (make-ring 20))
  ;;
  ;;; got to the definition and put it in a memory ring (by Roberto E. Vargas Caballero)
  (defun semantic-goto-definition (point)
    "Goto definition using semantic-ia-fast-jump
   save the pointer marker if tag is found"
    (interactive "d")
    (condition-case err
      (progn
        (ring-insert semantic-tags-location-ring (point-marker))
        (let* ((ctxt (semantic-analyze-current-context point))
                (pf (and ctxt (reverse (oref ctxt prefix))))
                (first (car pf)))
          (if (semantic-tag-p first)
            (semantic-ia--fast-jump-helper first)
            (semantic-complete-jump))))
      (error
        ;;if not found remove the tag saved in the ring
        (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
        (signal (car err) (cdr err)))))
  ;;
  ;;; go back with memory ring  (by Roberto E. Vargas Caballero)
  (defun semantic-pop-tag-mark ()
    "popup the tag save by semantic-goto-definition"
    (interactive)
    (if (ring-empty-p semantic-tags-location-ring)
      (message "%s" "No more tags available")
      (let* ((marker (ring-remove semantic-tags-location-ring 0))
              (buff (marker-buffer marker))
              (pos (marker-position marker)))
        (if (not buff)
          (message "Buffer has been deleted")
          (switch-to-buffer buff)
          (goto-char pos)))))
  ) ; (when section-mode-cedet-semantic

;;
;;;
;;;; BUFFER CYCLE
;;; browse in buffers without name start with * (by Claude TETE)
;; I don't use it
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.  User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "\\(^\\*\\|TAGS\\)" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))
;;; browse in buffers without name start with * (by Claude TETE)
;; I don't use it
(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.  User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "\\(^\\*\\|TAGS\\)" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;;
;;;
;;;; START UP
;;; run everything needed after start (needed with Emacs on MS Windows with
;;; maximize bug (by Claude TETE)
;; not used anymore (bug fixed)
(defun mystart-up ()
  "Start all mode necessary to work."
  (interactive)
  ;; to wait the maximize of the main window
  (custom-set-variables
   '(ecb-compile-window-height 0.25)
   '(ecb-windows-width 0.1))
  (ecb-activate)
  )
;;; run muse mode (by Claude TETE)
(defun mymuse-mode ()
  "Start muse mode."
  (interactive)
  (add-to-list 'load-path  (concat dotemacs-path "/plugins/muse-3.20/bin"))
  (setq load-path (cons (expand-file-name (concat dotemacs-path "/plugins/muse-3.20/bin")) load-path))
  (try-require 'muse-mode "    ")     ; load authoring mode
  (try-require 'muse-html "    ")     ; load publishing styles I use
  (try-require 'muse-latex "    ")

  (muse-derive-style "my-slides-pdf" "slides-pdf"
    :header (concat dotemacs-path "/plugins/themes/muse/header.tex")
    :footer  (concat dotemacs-path "/plugins/themes/muse/footer.tex")
    )
;  (muse-mode t)
  )

;;
;;;
;;;; CONFIG
;;; load config for awful people who cannot read code source on dark
;;; background and tiny font (by Claude TETE) deprecated
(defun cfg-noob ()
  "Configure GNU/Emacs for whose seem to want work."
  (interactive)
  ;; deprecated
  (load-file (concat dotemacs-path "/dotemacs/noob.el"))
  )
;;; load config for me after cfg-noob (by Claude TETE)
;; it do not revert all some bug...
(defun cfg-classic ()
  "Configure GNU/Emacs for whose want work."
  (interactive)
  (load-file (concat dotemacs-path "/emacs.el"))
  )

;;
;;;
;;;; SEARCH ERROR
;;; search a tab in buffer (by Claude TETE)
(defun search-tab ()
  "Search a tab in the current buffer."
  (interactive)
  (occur "[\t\v]")
)
;;; search a fault size in buffer (by Claude TETE)
(when (string= profile "Alstom Transport")
  (defun search-fault-size ()
    "Search a sizing fault in the current buffer."
    (interactive)
    ;; line more than 80 column
    (occur ".\\{81,\\}")
    )
  ) ; (when (string= profile "Alstom Transport")

;;
;;;
;;;; ALIGN RTRT MODE
(when section-mode-rtrt-script
  ;;; align "init" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-init (start end)
    "Align init variable test case (between START and END)."
    (interactive "r")
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\binit ") 1 1)
    )
  ;;; align "expected value" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-ev (start end)
    "Align expected value variable (between START and END)."
    (interactive "r")
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\bev ") 1 1)
    )
  ;;; align "expected value" and "init" in ptu script for rtrt (by Claude TETE)
  (defun rtrt-align-declaration (start end)
    "Align variable (between START and END)."
    (interactive "r")
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (rtrt-align-init start end)
    (rtrt-align-ev start end) ; sometimes it bugs the last ev is not align or two line after region is align
    )
  ;;; align "=" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-set (start end)
    "Align = (between START and END)."
    (interactive "r")
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "[-+=]\\{0,1\\}=") 1 1)
    )
  ) ; (when section-mode-rtrt-script

;;
;;;
;;;; FORMAT RTRT MODE
(when section-mode-rtrt-script
  ;;; remove whitespace before a colon in ptu script for RTRT (by Claude TETE)
  (defun rtrt-remove-whitespace-before-colon (start end)
    "Remove all space before a colon (between START and END)."
    (interactive "r")
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (replace-regexp "\\s-+," "," nil start end)
    )
  ;;; upcase "var" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-upcase-var-string (start end)
    "Upcase the var string (between START and END)."
    (interactive "r")
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (replace-regexp " [vV]ar " " VAR " nil start end)
    )
  ) ; (when section-mode-rtrt-script

;;
;;;
;;;; END OF LINE
;;; convert MS-DOS format \r\n to Unix format \n (by Claude TETE)
(defun dos2unix ()
  "Transform a DOS file to a Unix file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")
  )
)
;;; convert Unix format \n to MS-DOS format \r\n (by Claude TETE)
(defun unix2dos ()
  "Transform a Unix file to a DOS file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")
  )
)

;;
;;;
;;;; TRANSPARENCY
;;; toggle the window transparency to alpha 100->85->100... (by Claude TETE)
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  "Toggle the transparency of whole Emacs window."
  (interactive)
  (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
    (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;
;;;
;;;; SWAP/SPLIT WINDOWS
;; swap 2 windows (by Fabrice Niessen)
(defun my-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
          (message "You need exactly 2 windows to do this."))
    (t
      (let* ((w1 (first (window-list)))
              (w2 (second (window-list)))
              (b1 (window-buffer w1))
              (b2 (window-buffer w2))
              (s1 (window-start w1))
              (s2 (window-start w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)
        )
      )
    )
  )
;; toggle between horizontal and vertical split for 2 windows (by Fabrice
;; Niessen)
(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd (not (and (<= (car this-win-edges)
                                      (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                   (cadr next-win-edges)))))
            (splitter
	      (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
                'split-window-horizontally
		'split-window-vertically)
              )
            )
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1)))
      )
    )
  )

;;
;;;
;;;; SWITCH BUFFER
;;; Switching Between Two Recently Used Buffers (by Mathias Dahl)
;; like Alt+Tab in Windows Managers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  )

;;
;;;
;;;; (UN)COMMENT LINE
;;; (un)comment line if no region is marked
;; inspire from slick-copy
(defadvice comment-or-uncomment-region (before slick-copy activate compile)
  "When called interactively with no active region, (un)comment a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
               (line-beginning-position 2)))))

;;
;;;
;;;; SCROLL WITH KEEPING CURSOR
;;; Scroll the text one line down while keeping the cursor (by Geotechnical
;;; Software Services)
(defun scroll-down-keep-cursor ()
  (interactive)
  (scroll-down 1))
;;; Scroll the text one line up while keeping the cursor (by Geotechnical
;;; Software Services)
(defun scroll-up-keep-cursor ()
  (interactive)
  (scroll-up 1))

;;
;;;
;;;; MAXIMIZE
;;; maximize the current frame (the whole Emacs window) (by Claude TETE)
(defun frame-maximizer ()
  "Maximize the current frame"
  (interactive)
  (when running-on-ms-windows
    (w32-send-sys-command 61488))
  )

;;
;;;
;;;; TIME/DATE
;;; insert current date/time (by Scott McPeak)
; ----------------- insertion macros --------------------
; insert current date/time
;   %m   month in [01..12]
;   %-m  month in [1..12]
;   %d   day in [01..31]
;   %y   year in [00..99]
;   %Y   full year
;   %H   hour in [00..23]
;   %M   minute in [00..59]
; see format-time-string for more info on formatting options
(defun my-date-string ()
  (format-time-string "%Y-%m-%d"))
(defun insert-date ()
  "Insert time and date at cursor."
  (interactive)
  (insert (my-date-string)))

;;
;;;
;;;; WEB SEARCH
;;; translate from French to English in generic browser
(defun translate-fren (beg end)
  "Translate the word at point using WordReference or Google Translate when a region is selected."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (if (use-region-p)
        (browse-url-generic
          (concat "http://translate.google.fr/?hl=&ie=UTF-8&text=&sl=fr&tl=en#fr|en|"
            string))
        (browse-url-generic
          (concat "http://www.wordreference.com/fren/"
            string))
        )
      )
    )
  )
;;; translate from English to French in generic browser
(defun translate-enfr (beg end)
  "Translate the word at point using WordReference or Google Translate when a region is selected."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (if (use-region-p)
        (browse-url-generic
          (concat "http://translate.google.fr/?hl=&ie=UTF-8&text=&sl=en&tl=fr#en|fr|"
            string))
        (browse-url-generic
          (concat "http://www.wordreference.com/enfr/"
            string))
        )
      (message "No word at point or no mark set")
      )
    )
  )

;;; search in French Wikipedia
(defun wikipedia-fr (beg end)
  "Search the word at point or selected region using Wikipedia."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://fr.wikipedia.org/wiki/Special:Search?search=" string))
      (message "No word at point or no mark set")
      )
    )
  )
;;; search in English Wikipedia
(defun wikipedia-en (beg end)
  "Search the word at point or selected region using Wikipedia."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://en.wikipedia.org/wiki/Special:Search?search=" string))
      (message "No word at point or no mark set")
      )
    )
  )

;;; search synonym in French
(defun synonym-fr (beg end)
  "Search the word at point or selected region using Synonymes.com."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.synonymes.com/synonyme.php?mot=" string "&x=0&y=0"))
	  )
    )
  )

;;; search grammatical conjugation in French
(defun conjugation-fr (beg end)
  "Search the word at point or selected region using leconjugueur.com."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.leconjugueur.com/php5/index.php?v=" string))
      (message "No word at point or no mark set")
      )
    )
  )

;;; search in French Google
(defun google-fr (beg end)
  "Search the word at point or selected region using google.fr."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.google.fr/search?hl=fr&client=opera&hs=0HH&q=" string
          "&btnG=Rechercher&meta=&num=100&as_qdr=a&as_occt=any"))
      (message "No word at point or no mark set")
      )
    )
  )
;;; search in English Google
(defun google-en (beg end)
  "Search the word at point or selected region using google.fr."
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (browse-url-generic
        (concat "http://www.google.fr/search?hl=en&client=opera&hs=0HH&q=" string
          "&btnG=Rechercher&meta=&num=100&as_qdr=a&as_occt=any"))
      (message "No word at point or no mark set")
      )
    )
  )


;;
;;;
;;;; MAGNETI MARELLI
(when section-external-function-mm (message "    1.2.1 Functions Magneti Marelli...")
  (try-require 'function-mm "    ")
  (message "    1.2.1 Functions Magneti Marelli... Done"))

;;
;;;
;;;; ECB
(when section-mode-cedet-ecb
  (try-require 'function-ecb "    ")
  ) ; (when section-mode-cedet-ecb

;;
;;;
;;;; CLEARCASE
(when (or section-mode-clearcase section-mode-vc-clearcase)
  (try-require 'function-clearcase "    ")
  ) ; (when (or section-mode-clearcase section-mode-vc-clearcase)

;;
;;;
;;;; TEST (all after is for testing
(defun clt-test ()
  "Plein de test."
  (interactive)
  (message (thing-at-point 'word))
)

(setq semantic-c-takeover-hideif t)


;; start browser at url (by antonj from github)
(defun google-region (beg end)
  "Google the selected region."
  (interactive "r")
  (browse-url-generic (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" (buffer-substring beg end))))
(defun translate ()
  "Translate the word at point using WordReference."
  (interactive)
  (browse-url (concat "http://www.wordreference.com/fren/"
                (thing-at-point 'word))))

;;; Copy entire line in kill ring (without Home C-k C-y) (by Claude TETE)
(defun push-line ()
  "Select current line, push onto kill ring."
  (interactive)
  (save-excursion
    (copy-region-as-kill (re-search-backward "^") (re-search-forward "$"))
  )
)


;; This is what I bind to Alt-[ and Alt-]. (by Scott McPeak)
(defun find-matching-keyword ()
  "Find the matching keyword of a balanced pair."
  (interactive)
  (cond
    ; these first two come from lisp/emulation/vi.el
    ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
    ((looking-at "[])}]") (forward-char 1) (backward-sexp 1))

    ; TODO: Should the set of pairs be sensitive to the mode of
    ; the current file?

    ; Kettle CVC
    ((looking-at "ASSERT")
     (find-matching-element 're-search-forward 6 "ASSERT" "RETRACT"))
    ((looking-at "RETRACT")
     (find-matching-element 're-search-backward 0 "RETRACT" "ASSERT"))

    ; Kettle CVC
    ;
    ; "\\b": word boundary assertion, needed because one delimiter is
    ; a substring of the other
    ((looking-at "BLOCK")
     (find-matching-element 're-search-forward 5 "\\bBLOCK\\b" "ENDBLOCK"))
    ((looking-at "ENDBLOCK")
     (find-matching-element 're-search-backward 0 "ENDBLOCK" "\\bBLOCK\\b"))

    ; Simplify
    ((looking-at "BG_PUSH")
     (find-matching-element 're-search-forward 7 "BG_PUSH" "BG_POP"))
    ((looking-at "BG_POP")
     (find-matching-element 're-search-backward 0 "BG_POP" "BG_PUSH"))

    ; C/C++
    ((looking-at "#if")
     (find-matching-element 're-search-forward 3 "#if" "#endif"))
    ((looking-at "#endif")
     (find-matching-element 're-search-backward 0 "#endif" "#if"))

    ; ML
    ;
    ; this does not quite work because e.g. "struct" is also terminated
    ; with "end" ..
    ((looking-at "begin")
     (find-matching-element 're-search-forward 5 "\\bbegin\\b" "\\bend\\b"))
    ((looking-at "end")
     (find-matching-element 're-search-backward 0 "\\bend\\b" "\\bbegin\\b"))

    ;(t (error "Cursor is not on ASSERT nor RETRACT"))
    (t t)
  ))

;; Stefan Monnier . It is the opposite of fill-paragraph
;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;;; toggle fill-paragraph and "unfill" (by Xah Lee)
;; do not work
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property "stateIsCompact-p".
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
        (if (eq last-command this-command)
          (get this-command 'stateIsCompact-p)
          (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
        (if currentStateIsCompact
          (fill-region (region-beginning) (region-end))
          (let ((fill-column bigFillColumnVal))
            (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
          (let ((fill-column bigFillColumnVal))
          (fill-paragraph nil)
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

;;list-colors-display to display all color


(provide 'functions)

;;; functions.el ends here
