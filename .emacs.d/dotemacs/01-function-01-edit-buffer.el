;;; 01-function-01-edit-buffer.el --- add some function about edit buffer text -*- lexical-binding: t -*-

;; Copyright (c) 2006-2020 Claude Tete
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
;; Version: 6.5
;; Created: October 2006
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.custom function about edition of buffer text]
;; [SUBDEFAULT.t]
;;


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

(if (string-match-p tqnr-profile-name "thales")
  (defun llt_build_chain_insert_root ()
    (interactive)
    (if (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert "\n"
      "if not defined P_GIT (\n"
      "    echo \"ERREUR - Positionner la variable P_GIT\"\n"
      "    pause\n"
      "    exit /B\n"
      ")\n"
      "\n"
      "REM Extract current path\n"
      "SET CURRENT_PATH=%~dp0\n"
      "REM make sure it will run makefile next to this script\n"
      "CD %CURRENT_PATH%\n"
      "REM get root path of repository (path from makefile)\n"
      "SET VIEWROOT_PATH=%CURRENT_PATH%" (shell-command-to-string "pwd |  sed \"s=$(git rev-parse --show-toplevel)/==\" | sed \"s=[^/]*=..=g\" | sed \"s=/=\\\\\\=g\"")
      ))

  (defun llt_build_chain_insert_make ()
    (interactive)
    (if (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert "%P_GIT%\\bash.exe %VIEWROOT_PATH%\\build_chain\\%PLATFORM%\\make_with_env"))


  (defun insert-pragma-annotate ()
    "Insert a pragma annotate exempt on."
    (interactive)
    (beginning-of-line)
    (open-line 1)
    (insert "      pragma Annotate (gnatcheck, Exempt_On, \"Forbidden_Pragmas\", \"Handling of invalid value due to IRS design\");")
    (next-line 2)
    (beginning-of-line)
    (open-line 1)
    (insert "      pragma Annotate (gnatcheck, Exempt_Off, \"Forbidden_Pragmas\");")
    )

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; insert printf or ifdef for debug (used in epita kaneton project)
      (global-set-key         (kbd "<f9>")           'insert-pragma-annotate)
      (global-set-key         (kbd "<S-f9>")         'llt_build_chain_insert_make)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )

;; FitNesse debug point: Insertion of debug point
(when tqnr-section-mode-fitnesse
  ;; insert fit deubg point
  ;; !| FitnesseDebug |
  ;; | breakPoint | etapeId            |
  ;; | true       |  |
  (defun fitnesse-insert-debug ()
    "Insert a debug point."
    (interactive)
    (insert "\n!| FitnesseDebug |\n| breakPoint | etapeId |\n| true       |  |\n\n")
    (indent-according-to-mode)
    (backward-char 4))

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      (define-key fitnesse-mode-map     (kbd "<f11>")   'fitnesse-insert-debug)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  ) ;; (when tqnr-section-mode-fitnesse

(if (string-match-p tqnr-profile-name "sagem")
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
  )

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
    (global-set-key         (kbd "C-c C-;")             'comment-or-uncomment-region)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook

(defun comment-add-block-end ()
  "Add comment at end of a block"
  (interactive)
  (save-excursion
    (let (line bol eol)
      (save-excursion
        (find-matching-keyword)
        (beginning-of-line)
        (setq bol (point))
        (end-of-line)
        (setq eol (point))
        (setq line (buffer-substring bol eol)))
      (insert " ;; ")
      (insert line))))
;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; insert tag about coverage design document
    (global-set-key (kbd "C-c M-;") #'comment-add-block-end)
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

;;
;; ALIGN
(defun align-comma-separator (start end)
  "Align each element separated by comma character by using space.
When no region are selected, paragraph is the selected region."
  (interactive (tqnr-get-paragraph-position))
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (align-regexp start end ",\\(\\s-*\\)" 1 1 t))

(defun align-regexp-repeat (start end &optional regexp)
  "Align each element separated by REGEXP by using space.
When no region are selected, paragraph is the selected region."
  (interactive (tqnr-get-paragraph-position))
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (when (not regexp)
    (setq regexp (read-regexp "Align regexp: " (list "=>" ":=" "="))))
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))



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
    (while (search-forward "" nil t)
      (replace-match "'"))
    (goto-char (point-min))
    (while (search-forward "" nil t)
      (replace-match "+"))
    (goto-char (point-min))
    (while (search-forward "" nil t)
      (replace-match "-"))
    )
  )

;; replace tab with space + replace '+- non unicode
(defun clean-tav-gael ()
  "Clean files from TAV inside GAEL."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward "”" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "“" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "‘" nil t)
      (replace-match "'"))
    (goto-char (point-min))
    (while (search-forward "’" nil t)
      (replace-match "'"))
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

;; from https://www.emacswiki.org/emacs/DeletingWhitespace
(defun delete-horizontal-space-forward ()
  "Delete all space and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun delete-horizontal-space-backward ()
  "Delete all space and tabs before point."
  (interactive "*")
  (delete-horizontal-space t))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; delete all blank character or line except one
    (global-set-key     (kbd "M-SPC")   'just-one-space-or-line)
    ;; delete all space until next word
    (global-set-key     (kbd "M-S-SPC") 'delete-horizontal-space-forward)
    ;; delete all space until previous word
    (global-set-key     (kbd "H-SPC")   'delete-horizontal-space-backward)
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
