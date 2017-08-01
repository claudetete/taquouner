;;; 01-function-01-edit-buffer.el --- add some function about edit buffer text

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
;; Version: 6.2
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.custom function about edition of buffer text]
;;

;;; Change Log:
;; 2017-07-21 (6.2)
;;    split file into multiple files
;; 2016-09-28 (6.1)
;;    remove dash as symbol character + overload helm function about dir + add
;;    reverse string + multiple key about space remover
;; 2015-08-21 (6.0)
;;    add new web search + function to go in symref buffer
;; 2014-03-26 (5.9)
;;    add function to insert tag for reqtify in source code + remove old
;;    function of synergy
;; 2013-09-10 (5.8)
;;    add parameters and functions for synergy support
;; 2013-05-07 (5.7)
;;    condition on os detection for maximize function + do not run code for test
;; 2013-04-11 (5.6)
;;    add just-one-space-or-line function
;; 2013-03-29 (5.5)
;;    use url-hexify-string for web string + new function to indent function +
;;    add mixtab
;; 2013-02-05 (5.4)
;;    add function to get region or paragraph
;; 2012-12-27 (5.3)
;;    update dot emacs path + add bookmark in special buffer
;; 2012-11-30 (5.2)
;;    add switch to special buffer function
;; 2012-10-31 (5.1)
;;    try to use new navigate function (inconvenient)
;; 2012-10-26 (5.0)
;;    add double copy/kill (bind) will put in a register
;; 2012-10-18 (4.9)
;;    fix bug with resize window + try fix copy/kill with clipboard system
;; 2012-08-01 (4.8)
;;    add function to apply macro on region with same shortcut + add smart
;;    window resize + add change case on region with same shortcut + clean up
;; 2012-07-11 (4.7)
;;    add get line or region for interactive + split rtrt function file + try to
;;    fix annoying random behavior of completions buffer + fill region
;; 2012-07-09 (4.6)
;;    fix wait for fullscreen and add linux support + some test
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

;; [VARCOMMENT.FLAG DEBUG: used in kaneton project (epita)]
;; [VARIABLE.tqnr-function-kaneton nil]
(when tqnr-function-kaneton
  ;; insert printf for debug (by Kevin Prigent)
  ;;   printf("\n"); //DEBUG
  (defun printf-debug-shortcut ()
    "Insert a printf(\"n\"); //DEBUG."
    (interactive)
    (insert "printf(\"\\n\");//DEBUG")
    (indent-according-to-mode) (backward-char 12)
    )

  ;; insert a test of macro for debug (by Kevin Prigent)
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

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; insert printf or ifdef for debug (used in epita kaneton project)
      (global-set-key         [f7]                    'printf-debug-shortcut)
      (global-set-key         [f8]                    'ifdef-debug-shortcut)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  ) ;; (when tqnr-function-kaneton

;; insert a C comment to add tag for coverage
(defun tag-insert-shortcut ()
  "Insert a tag for coverage."
  (interactive)
  (let (my-module (my-buffer buffer-file-name))
    (when (string-match "/\\(...\\)[^/]*$" my-buffer)
      (setq my-module (upcase (match-string 1 my-buffer)))
      (beginning-of-line)
      (insert " /* -------------------------- */")
      (indent-according-to-mode)
      (insert "\n /* [COV.TAMBORIM_SDD_" my-module "_] */")
      (indent-according-to-mode)
      (insert "\n /* -------------------------- */")
      (indent-according-to-mode)
      (insert "\n")
      (search-backward my-module)
      (forward-char 4)
      )))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; insert tag about coverage design document
    (global-set-key         (kbd "<f9>")              'tag-insert-shortcut)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook

;;
;;
;; INDENT
;; indent the whole function (thanks to http://emacsredux.com/blog/2013/03/28/indent-defun/)
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; indent the whole function
    (global-set-key     (kbd "C-M-l")   'indent-defun)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;;
;; FILL
;; Change filling behavior
(put 'fill-region 'interactive-form
  '(interactive
     (if (use-region-p)
       (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2)))))

;;
;; MUSE
;; run muse mode (by Claude TETE)
(defun mymuse-mode ()
  "Start muse mode."
  (interactive)
  (add-to-list 'load-path  (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/muse-3.20/bin"))
  (try-require 'muse-mode "    ")     ; load authoring mode
  (try-require 'muse-html "    ")     ; load publishing styles I use
  (try-require 'muse-latex "    ")
  ;;
  (muse-derive-style "my-slides-pdf" "slides-pdf"
    :header (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes/muse/header.tex")
    :footer  (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/themes/muse/footer.tex")
    )
;  (muse-mode t)
  )

;;
;; (UN)COMMENT LINE
;; (un)comment line if no region is marked (no more used, replace by comment-dwim2)
;; inspire from slick-copy
(defadvice comment-or-uncomment-region (before slick-copy activate compile)
  "When called interactively with no active region, (un)comment a single
line instead."
  (interactive
    (if mark-active
      (list (region-beginning) (region-end))
      (list (line-beginning-position)
               (line-beginning-position 2)))))
;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; (un)comment region
    (global-set-key         (kbd "H-/")             'comment-or-uncomment-region)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook

;;
;; TIME/DATE
;; insert current date/time (by Scott McPeak)
;; ----------------- insertion macros --------------------
;; insert current date/time
;;   %m   month in [01..12]
;;   %-m  month in [1..12]
;;   %d   day in [01..31]
;;   %y   year in [00..99]
;;   %Y   full year
;;   %H   hour in [00..23]
;;   %M   minute in [00..59]
;; see format-time-string for more info on formatting options
(defun my-date-string ()
  (format-time-string "%Y-%m-%d"))

(defun insert-date ()
  "Insert time and date at cursor."
  (interactive)
  (insert (my-date-string)))

;; insert date (format YYYY-MM-DD)
(defalias 'id 'insert-date)


;;
;; CASE
;; uppercase the region or the following word
(defun case-up ()
  (interactive)
  (if (use-region-p)
    (upcase-region (region-beginning) (region-end))
    (upcase-word 1)
    )
  )

;; downcase the region or the following word
(defun case-down ()
  (interactive)
  (if (use-region-p)
    (downcase-region (region-beginning) (region-end))
    (downcase-word 1)
    )
  )

;; uppercase the first character and down the rest of the region or the following word
(defun case-capitalize ()
  (interactive)
  (if (use-region-p)
    (capitalize-region (region-beginning) (region-end))
    (capitalize-word 1)
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; upper case word or region
    (global-set-key     (kbd "M-u")     'case-up)
    ;; down case word or region
    (global-set-key     (kbd "M-l")     'case-down)
    ;; capitalize case word or region
    (global-set-key     (kbd "M-c")     'case-capitalize)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(defun reverse-string (beg end)
  "Reverse the str where str is a string"
  (interactive (clt-get-string-position))
  (let ((string (buffer-substring beg end)))
    (if string
      (delete-region beg end)
      (message (concat "=" string "="))
      (insert (string-reverse string))
      )))

;;
;; MIXTAB
;; mix between smart tab and tabkey2 to have tab key OK and double tab key to
;; fold (some code from smart-tab.el and pc-keys.el) (not used)
(when tqnr-section-mode-fold-dwim
  (defun mixtab-indent ()
    "Indent region if mark is active, or current line otherwise."
    (interactive)
    (if smart-tab-debug
      (message "default"))
    (if (use-region-p)
      (indent-region (region-beginning)
        (region-end))
      (indent-for-tab-command)))

  ;; bind to tab key
  (defun mixtab ()
    "First hitting key indent even if selection, second in a row fold the source
at the point."
    (interactive)
    (let* ((keys (recent-keys))
            (len (length keys))
            (key1 (if (> len 0) (elt keys (- len 1)) nil))
            (key2 (if (> len 1) (elt keys (- len 2)) nil))
            (key-equal-1 (equal key1 key2)))
      (if (and section-mode-fold-dwim key-equal-1)
        (fold-dwim-toggle)
        (tab-indent))))
  ) ; (when tqnr-section-mode-fold-dwim

;; replace tab with space + replace '+- non unicode
(defun clean-at ()
  "Clean files from AT."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward "’" nil t)
      (replace-match "'"))
    (while (search-forward "•" nil t)
      (replace-match "+"))
    (while (search-forward "–" nil t)
      (replace-match "-"))
    )
  )

;;
;; DELETE
(defun just-one-space-or-line ()
  "Delete spaces exept one if there is a print character on the line otherwise
delete blank lines"
  (interactive)
  (save-excursion
    (let* ((start (point))
            (end)
            (line-start (line-beginning-position))
            (line-end (line-beginning-position 2))
            (keys (recent-keys))
            (len (length keys))
            (key1 (if (> len 0) (elt keys (- len 1)) nil))
            (key2 (if (> len 1) (elt keys (- len 2)) nil))
            (key-equal-1 (equal key1 key2)))
      (cond
        (key-equal-1
          (message "second")
          (skip-chars-backward " \t")
          (delete-char 1))
        (t
          (message "first")
          ;; get end of line or start of next word
          (skip-chars-forward " \t")
          (when (looking-at "\r")
            (forward-char))
          (when (looking-at "\n")
            (forward-char))
          (setq end (point))
          ;; go back
          (goto-char start)
          ;; get start of line or end of previous word
          (skip-chars-backward " \t")
          (setq start (point))
          (if (and (eq start line-start) (eq end line-end))
            (progn
              (just-one-space 0)
              (delete-blank-lines))
            (just-one-space)))))))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; delete all blank character or line except one
    (global-set-key     (kbd "M-SPC")   'just-one-space-or-line)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;; google code wiki to tex
(defun wiki2tex ()
  (interactive)
  (save-excursion
    (let ((start (point-min)) (end (point-max)))
      ;; table to item
      (replace-regexp "^||\\s-+" "\\\\cm{" nil start end)
      (replace-regexp "\\s-+|| " "}{" nil start end)
      (replace-regexp "\\s-+||$" "}" nil start end)
      ;; chapter to section
      (replace-regexp "^= " "\\\\section{" nil start end)
      (replace-regexp " =$" "}" nil start end)
      ;; title to section
      (replace-regexp "^=== " "\\\\subsection{" nil start end)
      (replace-regexp " ===$" "}" nil start end)
      ;; subtitle to subsection
      (replace-regexp "^===== " "\\\\subsubsection{" nil start end)
      (replace-regexp " =====$" "}" nil start end)
      ;; remove separator line
      (replace-regexp "^\\s-*----\\s-*$" "" nil start end)
      ))
  )

;; from http://emacs.stackexchange.com/a/11049 by Jordon Biondo
(defun rectangle-number-lines-wrapper (start end start-at step &optional format)
  "Insert numbers in front of the region-rectangle.

START-AT, if non-nil, should be a number from which to begin
counting.  STEP, if non-nil should be a number to increment by.
FORMAT, if non-nil, should be a format string to pass to `format'
along with the line count.  When called interactively with a
prefix argument, prompt for START-AT, STEP, and FORMAT."
  (interactive
   (if (not current-prefix-arg)
     (let* ((start (region-beginning))
             (end   (region-end))
             (start-at (read-number "Number to count from: " 1))
             (step (read-number "Step: " 1)))
       (list start end start-at step
         (read-string "Format string: "
           (rectangle--default-line-number-format
             start end start-at))))
     (list (region-beginning) (region-end) 1 1 nil)))
  (cl-letf (((symbol-function 'rectangle-number-line-callback)
              `(lambda (start _end format-string)
                 (move-to-column start t)
                 (insert (format format-string rectangle-number-line-counter))
                 (setq rectangle-number-line-counter
                   (+ rectangle-number-line-counter ,step)))))
    (rectangle-number-lines start end start-at format)))


(provide '01-function-01-edit-buffer)

;;; 01-function-01-edit-buffer.el ends here
