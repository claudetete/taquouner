;;; function.el --- a config file to add some function

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
;; Version: 3.6
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-external-functions'
;;
;; it need to be split...

;;; Change Log:
;; 2012-05-02 (3.6)
;;    add function about isearch, macro, windows swap + comment for fix
;;    fullscreen bug
;; 2012-03-29 (3.5)
;;    add function align with =
;; 2012-03-28 (3.4)
;;    translate comments in english and change old format
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
;;;  CONST
(defconst clt-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")

(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    (defconst clt-nsf-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -DC_COMP_COSMIC_MC9S12 -D__MC9S12xx__ -D__MC9S12XHZ__ -I d:/ccm_wa/NSF/NSF/NSF_CLIENT/out/include")

    (defconst clt-nbnfhl-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/NBNF/NBNF_HL/NBNF_CLIENT_HL/out/include")

    (defconst clt-nbnfll-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -DC_COMP_COSMIC_MC9S12 -D__MC9S12xx__ -D__MC9S12XHZ__ -I d:/ccm_wa/NBNF/NBNF_LL/NSFNBNF_CLIENT/out/include")

    (defconst clt-ecar-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/ECAR/ENSF_CLIENT/out/include")

    (defconst clt-xl1-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/XL1/XL1_CLIENT/out/include")
    ) ; Magneti Marelli

  ) ; cond ---------------------------------------------------------------------

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
;;;; SELECT WORD
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
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)
    )
  )

;;
;;;
;;; COPY/CUT
;; Change cutting behavior (by Fabrice Niessen):
;; "Many times you'll do a kill-line command with the only intention of
;; getting the contents of the line into the killring. Here's an idea stolen
;; from Slickedit, if you press copy or cut when no region is active, you'll
;; copy or cut the current line."
;; <http://www.zafar.se/bkz/Articles/EmacsTips>
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;
;;;
;;; SEARCH
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

;; occur when incremental search (by Fabrice Niessen)
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

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
;;; MACRO
;; toogle macro recording on/off (by Fabrice Niessen)
(defun my-toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))
;; toogle macro recording on/off (by Fabrice Niessen)
(defun my-toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)
  (end-kbd-macro))
;; shortcuts are put in shortcut-function.el

;;
;;;
;;; GREP-FIND
(cond
;; Magneti Marelli -------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    ;;; grep custom (by Claude TETE)
    (defun nsf-grep-find ()
      "Search word (egrep regex) in all files for NSF project."
      (interactive)
      (let (word prompt input)
        (setq word (clt-select-word))
        (if word
          (setq prompt (concat "Grep word NSF project: (default " word ") "))
          (setq prompt "Grep word (NSF): "))
        (setq input (completing-read prompt 'try-completion nil nil nil nil))
        (if (not (equal "" input))
          (setq word input))
        (grep (concat "cat d:/ccm_wa/NSF/NSF/NSF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\"")))
      )

    ;;; grep custom (by Claude TETE)
    (defun nll-grep-find ()
      "Search word (egrep regex) in all files for NBNF LL project."
      (interactive)
      (let (word prompt input)
        (setq word (clt-select-word))
        (if word
          (setq prompt (concat "Grep word NBNF_LL project: (default " word ") "))
          (setq prompt "Grep word (NBNF_LL): "))
        (setq input (completing-read prompt 'try-completion nil nil nil nil))
        (if (not (equal "" input))
          (setq word input))
        (grep (concat "cat d:/ccm_wa/NBNF/NBNF_LL/NSFNBNF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\""))
        )
      )

    ;;; grep custom (by Claude TETE)
    (defun nhl-grep-find ()
      "Search word (egrep regex) in all files for NBNF_HL project."
      (interactive)
      (let (word prompt input)
        (setq word (clt-select-word))
        (if word
          (setq prompt (concat "Grep word NBNF_HL project: (default " word ") "))
          (setq prompt "Grep word (NBNF_HL): "))
        (setq input (completing-read prompt 'try-completion nil nil nil nil))
        (if (not (equal "" input))
          (setq word input))
        (grep (concat "cat d:/ccm_wa/NBNF/NBNF_HL/NBNF_CLIENT_HL/*.files | xargs grep --color=always -nsIE \""word"\""))
        )
      )

    ;;; grep custom (by Claude TETE)
    (defun ecar-grep-find ()
      "Search word (egrep regex) in all files for ECAR project."
      (interactive)
      (let (word prompt input)
        (setq word (clt-select-word))
        (if word
          (setq prompt (concat "Grep word ECAR project: (default " word ") "))
          (setq prompt "Grep word (ECAR): "))
        (setq input (completing-read prompt 'try-completion nil nil nil nil))
        (if (not (equal "" input))
          (setq word input))
        (grep (concat "cat d:/ccm_wa/ECAR/ENSF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\""))
        )
      )
    ) ; Magneti Marelli
  ) ; cond ---------------------------------------------------------------------

;;
;;;
;;;; ECB
;;; increase/decrease width of ecb window (by Claude TETE)
(defun ecb-toggle-width ()
  "Toggle variable ecb-window-width between 10% and 25%."
  (interactive)
  (set-variable 'ecb-windows-width (if (= ecb-windows-width 0.1) 0.25 0.1))
  (ecb-redraw-layout)
  (message "Ecb width set to %d." ecb-windows-width)
  )
;;; open ecb history when ecb window is hide (by Claude TETE)
(defun ecb-myopen-history ()
  "Open ecb-history."
  (interactive)
  ;;(ecb-show-ecb-windows) ; this is bugged
  (ecb-goto-window-history)
  (message "Ecb History.")
  )

;;; open ecb tree directory when ecb window is hide (by Claude TETE)
(defun ecb-myopen-directories ()
  "Open ecb-directories."
  (interactive)
  ;;(ecb-show-ecb-windows) ; this is bugged
  (ecb-goto-window-directories)
  (message "Ecb Directories.")
  )

;;; open ecb source list when ecb window is hide (by Claude TETE)
(defun ecb-myopen-sources ()
  "Open ecb-sources."
  (interactive)
  ;;(ecb-show-ecb-windows) ; this is bugged
  (ecb-goto-window-sources)
  (message "Ecb Sources.")
  )

;;; open ecb function list when ecb window is hide (by Claude TETE)
(defun ecb-myopen-methods ()
  "Open ecb-methods."
  (interactive)
  ;;(ecb-show-ecb-windows) ; this is bugged
  (ecb-goto-window-methods)
  (message "Ecb Methods.")
  )


;;
;;;
;;;; SEMANTIC
(when section-mode-cedet-semantic
 (defvar semantic-tags-location-ring (make-ring 20))

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
   '(ecb-compile-window-height 25)
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
  (muse-mode t)
  )


;;
;;;
;;;; CONFIG
;;; load config for awful people who cannot read code source on dark
;;; background and tiny font (by Claude TETE)
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

;;; attempt to load a feature/library, failing silently (by Fabrice Niessen)
;; patched by Claude TETE to add string before message
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature &optional indent)
  "Attempt to load a library or module (FEATURE).  Return true if the library \
given as argument is successfully loaded.  If not, instead of an error, just \
add the package to a list of missing packages. INDENT contains string to add \
before message."
  (let ()
    (if (eq indent nil) (setq indent ""))
  (condition-case err
      ;; protected form
      (progn
        (message "%sChecking for library `%s'..." indent feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "%sChecking for library `%s'... Found" indent feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "%sChecking for library `%s'... Missing" indent feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil))))

;;
;;;
;;;; SEARCH ERROR
;;; search a tab in buffer (by Claude TETE)
(defun search-tab ()
  "Search a tab in the current buffer."
  (interactive)
  (occur "[\t\v]")
)

(cond
  ;; Magneti Marelli------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    ;;; search a fault in buffer (by Claude TETE)
    (defun search-fault ()
      "Search a fault in the current buffer."
      (interactive)
      ;; no space after ( after keyword
      (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\)[ ]*([ ]+" 'hi-green)
      ;; no space before ) after keyword
      (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\)[ ]*([^()]*[ ]+)" 'hi-green)
      ;; missing space after keyword
      (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\|#if\\|#endif\\)[^ ]*(" 'hi-yellow)
      ;;;; too space after/before parentheses
      ;;(highlight-regexp "^[^/\\n]*([ ]\\{1,\\}" 'hi-yellow)
      ;;(highlight-regexp "^[^/\\n]*[ ]\\{1,\\})" 'hi-yellow)
      (highlight-regexp "^[^/\\n]*[A-Za-z0-9]\\{4,7\\}_\\w[ ]\\{1,\\}(.*[),]" 'hi-yellow)
      ;; wrong type used
      (highlight-regexp "^[^/\\n]*[^us_]\\(byte\\|short\\|long\\))" 'hi-pink)
      ;; space before a ( for a call/declaration of function
      (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]+(" 'hi-yellow)
      ;; space after a ( for a call/declaration of function
      (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]*([ ]+[[:alnum:]_]*" 'hi-green)
      ;; space before a ) for a call/decalaration of function
      (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]*([ ]*[[:alnum:]_]*\\([ ]+[[:alnum:]_]*\\|[ ]*([[:alnum:]_]*\\(,[[:alnum:]_]\\)*)\\)[ ]+)" 'hi-green)
      )

    ;;; search a fault size in buffer (by Claude TETE)
    (defun search-fault-size ()
      "Search a sizing fault in the current buffer."
      (interactive)
      ;; wrong function header used
      (highlight-regexp "/\\**/" 'hi-blue)
      ;; line more than 80 column
      (highlight-regexp ".\\{81,\\}" 'hi-green)
      )
    ) ; Magneti Marelli

  ) ; cond ---------------------------------------------------------------------


;;
;;;
;;;; COLOR TRACE CAN
(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    ;;; color CAN trace for immo (by Claude TETE)
    (defun color-trace-can-immo ()
      "Color the trace in the current buffer."
      (interactive)
      ;; Response OK and NOK from IMMO
      (highlight-regexp " 0?17[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6\\}[ ]*[0-9A-F][13579BDF]" 'font-lock-function-name-face)
      (highlight-regexp " 0?17[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6\\}[ ]*[0-9A-F][02468ACE]" 'font-lock-preprocessor-face)
      ;; request from ELV
      (highlight-regexp " 0?16[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-comment-face)
      ;; response from KESSY
      (highlight-regexp " 29D[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6,\\}[ ]*[1-9A-F][0-9A-F]  [0-9A-F][0-9A-F]" 'font-lock-variable-name-face)
      (highlight-regexp " 29D[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-variable-name-face)
      ;; request from IMMO
      (highlight-regexp " 155[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-string-face)
      (highlight-regexp " 157[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-string-face)
      ;; button start
      (highlight-regexp " 5F1[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*[0-9A-F][89A-F]" 'font-lock-type-face)
      ;; KL 15 ON
      (highlight-regexp " 5F1[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
      ;; KL S-Contact ON ;;
      (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF]" 'font-lock-keyword-face)
      ;; not recognized key
      (highlight-regexp " 621[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}[ ]*[0-9A-F]0" 'compilation-warning)
      ;; recognised key
      (highlight-regexp " 621[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}[ ]*[0-9A-F][1-9A-F]" 'font-lock-function-name-face)
      )

    ;;; color CAN trace for cv sensor (by Claude TETE)
    (defun color-trace-can-cvsensor ()
      "Color the trace in the current buffer."
      (interactive)
      ;; BRAKE
      (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*8[0-9A-F]" 'font-lock-type-face)
      ;; error
      (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*[9AB][0-9A-F]" 'font-lock-comment-face)
      ;; OFF
      (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*C[0-9A-F]" 'font-lock-preprocessor-face)
      ;; ON
      (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*D[0-9A-F]" 'font-lock-function-name-face)
      ;;
      ;; KL 15 ON
      (highlight-regexp " 575[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
      ;; KL 15 ON
      (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
      )

    ;;; color CAN trace for NMH (by Claude TETE)
    (defun color-trace-can-nmh ()
      "Color the trace in the current buffer."
      (interactive)
      ;; NMH message
      (highlight-regexp " 72[4-9A-F][ ]*Tx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-type-face)
      (highlight-regexp " 73[0-9A-F][ ]*Tx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-type-face)
      ;;
      ;; comment state
      (highlight-regexp "\"    START       \"" 'font-lock-variable-name-face)
      (highlight-regexp "\" READY_TO_SLEEP \"" 'font-lock-variable-name-face)
      (highlight-regexp "\"PREPARE_TO_SLEEP\"" 'font-lock-variable-name-face)
      (highlight-regexp "\"    SLEEP       \"" 'font-lock-variable-name-face)
      (highlight-regexp "\"    NORMAL      \"" 'font-lock-variable-name-face)
      ;;
      (highlight-regexp "/\\*.*\\*/" 'font-lock-comment-face)
      ;;
      ;; State of kombi bus
      ;;  Sleep -> Start            (pale green)
      (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]1\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-function-name-face)
      ;;
      ;;  PrepareToSleep -> Start   (cyan)
      (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]2\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-keyword-face)
      ;;
      ;;  Start -> Normal           (pale turquoise)
      (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]4\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'font-lock-constant-face)
      ;;
      ;;  ReadyToSleep -> Normal    (pale grey blue)
      (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]8\\([ ]*[0-9A-F][0-9A-F]\\)\\{3\\}" 'fonct-lock-builtin-face)
      )

    ;;; color CAN trace for current problem (by Claude TETE)
    (defun color-trace-can-curr ()
      "Color the trace in the current buffer."
      (interactive)
      ;; mBSG_Kombi message KL58d != 0
      (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[1-9A-F][0-9A-F]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-type-face)
      (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[0-9A-F][1-9A-F]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-type-face)
      ;;
      ;; mBSG_Kombi message KL58d = 0
      (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*00\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-variable-name-face)
      ;;
      ;; mBSG_Last KL15 ON
      (highlight-regexp " 570[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-string-face)
      ;;
      ;; mBSG_Last KL15 OFF
      (highlight-regexp " 570[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][014589CD]\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-constant-face)
      ;;
      ;; mSystem_info1
      (highlight-regexp " 5D0[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][4567CDEF]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-comment-face)
      ;;
      ;; comment
      (highlight-regexp "/\\*.*\\*/" 'font-lock-comment-face)
      ;;
      ;; State of kombi bus
      ;;  Sleep -> Start            (pale green)
      (highlight-regexp " 727[ ]*[RT]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]1\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-function-name-face)
      ;;
      ;;  PrepareToSleep -> Start   (cyan)
      (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]2\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-keyword-face)
      ;;
      ;;  Start -> Normal           (pale turquoise)
      (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]4\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'font-lock-constant-face)
      ;;
      ;;  ReadyToSleep -> Normal    (pale grey blue)
      (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]8\\([ ]*[0-9A-F][0-9A-F]\\)\\{3\\}" 'fonct-lock-builtin-face)
      )

    ;;; color CAN trace for fuel tank (by Claude TETE)
    (defun color-trace-can-fuel ()
      "Color the trace in the current buffer."
      (interactive)
      ;; mKombi_1
      ;; Tankinhalt
      (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[1-9A-F][0-9A-F]" 'font-lock-comment-face)
      (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[0-9A-F][1-9A-F]" 'font-lock-comment-face)
      ;; Tankstop
      (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF][ ]*[89ABCDEF]" 'font-lock-constant-face)
      ;; Sta_tank
      (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF]" 'font-lock-function-name-face)
      ;; Warn_tank
      (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[4567CDEF][0-9A-F]" 'font-lock-type-face)
      ;;
      ;; mMFA_1
      ;; Reichweite
      (highlight-regexp " 629[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-preprocessor-face)
      ;;
      ;; mMFA_1
      ;; Verbrauch
      (highlight-regexp " 62B[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'compilation-warning)
      ;;
      ;; KL 15 ON
      (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
      )

    ;;; color CAN trace for electric range (by Claude TETE)
    (defun color-trace-can-erange ()
      "Color the trace in the current buffer."
      (interactive)
      ;; battery charge in percent in mMotor_EV1
      (highlight-regexp " 61[Aa][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}[ ]*[0-9A-F][0-9A-F]" 'font-lock-comment-face)
      ;; battery charge in kWh in mMotor_EV1
      (highlight-regexp " 61[Aa][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}[ ]*[0-9A-F][0-9A-F][ ]*[0-9A-F][0-9A-F]" 'font-lock-type-face)
      ;; consumption in mMotor_15
      (highlight-regexp " 57[Ff][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{8\\}" 'font-lock-function-name-face)
      ;;
      ;; KL 15 ON
      (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
      )
    ) ; Magneti Marelli

  ) ; cond ---------------------------------------------------------------------

;;
;;;
;;;; ALIGN RTRT MODE
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

;;
;;;
;;;; FORMAT RTRT MODE
;;; remove withespace before a colon in ptu script for RTRT (by Claude TETE)
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
;;;; C-EXPAND-MACRO
(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    ;;; expand macro for project NSF (by Claude TETE)
    (defun nsf-c-expand-macro (start end)
      "Expand macro in C language for the NSF project (between START and END)."
      (interactive "r")
      (unless (and start end)
        (error "The mark is not set now, so there is no region"))
      (setq c-macro-cppflags clt-nsf-flag)
      (c-macro-expand start end nil)
      )

    ;;; expand macro for project NBNF HL (by Claude TETE)
    (defun nhl-c-expand-macro (start end)
      "Expand macro in C language for the NBNF_HL project (between START and END)."
      (interactive "r")
      (unless (and start end)
        (error "The mark is not set now, so there is no region"))
      (setq c-macro-cppflags clt-nbnfhl-flag)
      (c-macro-expand start end nil)
      )

    ;;; expand macro for project NBNF LL (by Claude TETE)
    (defun nll-c-expand-macro (start end)
      "Expand macro in C language for the NBNF_LL project (between START and END)."
      (interactive "r")
      (unless (and start end)
        (error "The mark is not set now, so there is no region"))
      (setq c-macro-cppflags clt-nbnfll-flag)
      (c-macro-expand start end nil)
      )

    ;;; expand macro for project ENSF (by Claude TETE)
    (defun ecar-c-expand-macro (start end)
      "Expand macro in C language for the ECAR project (between START and END)."
      (interactive "r")
      (unless (and start end)
        (error "The mark is not set now, so there is no region"))
      (setq c-macro-cppflags clt-ecar-flag)
      (c-macro-expand start end nil)
      )

    ;;; expand macro for project XL1 (by Claude TETE)
    (defun xl1-c-expand-macro (start end)
      "Expand macro in C language for the XL1 project (between START and END)."
      (interactive "r")
      (unless (and start end)
        (error "The mark is not set now, so there is no region"))
      (setq c-macro-cppflags clt-xl1-flag)
      (c-macro-expand start end nil)
      )
    ) ; Magneti Marelli

  ) ; cond ---------------------------------------------------------------------

;;
;;;
;;; COMPLETIONS
;; expand text trying various ways to find its expansion (by Fabrice Niessen)
(when (try-require 'hippie-exp)
  ;; list of expansion functions tried (in order) by `hippie-expand'
  (setq hippie-expand-try-functions-list
    '(
       try-expand-dabbrev   ; from current buffer
       try-expand-dabbrev-visible   ; from visible parts of all windows
       try-expand-dabbrev-all-buffers   ; from all other buffers
       try-expand-dabbrev-from-kill
       try-complete-file-name-partially
       try-complete-file-name
       try-expand-all-abbrevs
       try-expand-list
       try-expand-line
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol
       try-expand-whole-kill
       )
    )

  ;; expand-function (by Fabrice Niessen)
  (defun my-hippie-expand (arg)
    ;; called with a positive prefix `P', it jumps directly to the `P'-th
    ;; `try-function'
    (interactive "P")
    ;; `hippie-expand' does not have a customization-feature (like
    ;; `dabbrev-expand') to search case-sensitive for completions. So we
    ;; must set `case-fold-search' temporarily to nil!
    (let ((old-case-fold-search case-fold-search))
      (setq case-fold-search nil)
      (hippie-expand arg)
      (setq case-fold-search old-case-fold-search)
      )
    )
)

;;
;;;
;;; SWAP/SPLIT WINDOWS
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
;;;; TEST (all after is for testing
(defun clt-test ()
  "Plein de test."
  (interactive)
  (insert (concat "toto=" shell-file-name "$"))
)

(setq semantic-c-takeover-hideif t)

(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    (fset 'header-function
      [home right right ?\C-7 ?\C-6 delete ?\C-7 ?\C-6 kp-subtract home])
    (global-set-key (kbd "C-c '") 'header-function)

    (fset 'delete-space-before-function-parenthese
      [home ?\C-s ?\( left backspace home])
    (global-set-key (kbd "C-c ;") 'delete-space-before-function-parenthese)
    ) ; Magneti Marelli

  ) ; cond ---------------------------------------------------------------------


;;; Copy entire line in kill ring (whitout Home C-k C-y) (by Claude TETE)
(defun push-line ()
  "Select current line, push onto kill ring."
  (interactive)
  (save-excursion
    (copy-region-as-kill (re-search-backward "^") (re-search-forward "$"))
  )
)

;;;
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


;;;; from guillaume salagnac

;(global-set-key (kbd "<C-S-up>") 'scroll-one-down)
;(global-set-key (kbd "<C-S-down>") 'scroll-one-up)

(defun scroll-one-up ()
        (interactive)
        (scroll-up 1))

(defun scroll-one-down ()
        (interactive)
        (scroll-up -1))

;; Pour par taper sur 'inser' par erreur
(put 'overwrite-mode 'disabled t)

;;list-colors-display to display all color


;; S-RET leaves lazy-highlighted matches.
;(defun my-isearch-exit-leave-lazy-highlight ()
;  "Exit search and leave extra match highlighting."
;  (interactive)
;  (let ((lazy-highlight-cleanup nil))
;    (when isearch-lazy-highlight
;      (isearch-lazy-highlight-new-loop (point-min) (point-max)))
;    (isearch-exit)))

;(define-key isearch-mode-map [(shift return)]
;                             'my-isearch-exit-leave-lazy-highlight)

;; C-RET doesn't add the current search string to the history.
;(add-hook 'isearch-mode-end-hook
;          (lambda ()
;            ;; On typing C-RET
;            (when (eq last-input-char 'C-return)
;              ;; Set the point at the beginning of the search string
;              (if (and isearch-forward isearch-other-end)
;                  (goto-char isearch-other-end))
;              ;; Don't push the search string into the search ring
;              (if isearch-regexp
;                  (setq regexp-search-ring (cdr regexp-search-ring))
;                (setq search-ring (cdr search-ring))))))

;; Use ScrollLock key to activate scroll-lock-mode
;; (from http://lists.gnu.org/archive/html/emacs-devel/2005-06/msg01274.html)
;(let ((key (if (eq window-system 'w32) "<scroll>" "<Scroll_Lock>")))
;  (unless (lookup-key (current-global-map) (read-kbd-macro key))
;    (define-key (current-global-map) (read-kbd-macro key) 'scroll-lock-mode)))

;;(global-set-key (kbd "C-c t") 'toggle-transparency)

;; I don't have tested
;;; cyclin through multiple tags results (by Kevin Rodgers)
;;(when (locate-library "gtags")
;;
;;  (autoload 'gtags-mode "gtags" nil t)
;;
;;  (when (executable-find "global")
;;
;;    (defadvice gtags-visit-rootdir (after make-complete-list activate)
;;      "Rebuilds completion list when changing GLOBAL database rootdir."
;;      (gtags-make-complete-list))
;;
;;    (defun gtags-global-dir-p (dir)
;;      "Return non-nil if directory DIR contains a GLOBAL database."
;;      (and (file-exists-p (expand-file-name "GPATH" dir))
;;           (file-exists-p (expand-file-name "GRTAGS" dir))
;;           (file-exists-p (expand-file-name "GSYMS" dir))
;;           (file-exists-p (expand-file-name "GTAGS" dir))))
;;
;;    (defun gtags-global-dir (&optional dir)
;;      "Return the nearest super directory that contains a GLOBAL database."
;;      (interactive)
;;      (when (null dir)
;;        (setq dir default-directory))
;;      (cond ((gtags-global-dir-p dir) dir)
;;            ((equal (file-truename dir) (file-truename "/")) nil)
;;            (t (gtags-global-dir (file-name-as-directory
;;                                  (expand-file-name ".." dir))))))
;;
;;    (defvar gtags-global-complete-list-obsolete-flag nil
;;      "When non-nil, the GLOBAL complete list should be rebuilt.")
;;
;;    (defun gtags-global-update ()
;;      "If current directory is part of a GLOBAL database update it."
;;      (interactive)
;;      (when (gtags-global-dir)
;;        (if (equal (call-process "global" nil nil nil "-vu") 0)
;;            (setq gtags-global-complete-list-obsolete-flag t)
;;          (error "Global database update failed"))))
;;
;;    (defun gtags-global-complete-list-maybe ()
;;      "Rebuild the GLOBAL complete list when indicated.
;;See `gtags-global-complete-list-obsolete-flag'."
;;      (interactive)
;;      (when gtags-global-complete-list-obsolete-flag
;;        (gtags-make-complete-list)
;;        (setq gtags-global-complete-list-obsolete-flag nil)))
;;
;;    (add-hook 'gtags-mode-hook
;;              (lambda ()
;;                (add-hook 'after-save-hook 'gtags-global-update nil t)
;;                (defadvice gtags-find-tag
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-rtag
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-symbol
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-pattern
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-with-grep
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-with-idutils
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-file
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-parse-file
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                (defadvice gtags-find-tag-from-here
;;                  (before gtags-global-complete-list-maybe activate)
;;                  (gtags-global-complete-list-maybe))
;;                )                       ; (lambda () ...)
;;              )                        ; (add-hook 'gtags-mode-hook ...)
;;    )                            ; (when (executable-find "global") ...)
;;
;;  ;; Use gtags in all modes for now.
;;  (gtags-mode 1)
;;  )                                ; (when (locate-library "gtags") ...)
;;

;;; functions.el ends here
