;;; mode.el --- a config file for all mode settings

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

;; Keywords: config, mode
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.2
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-mode'
;;              var     `section-external-directory'

;;; Change Log:
;; 2012-05-10 (2.2)
;;    add org mode (not used)
;; 2012-05-04 (2.1)
;;    add dired sort mode
;; 2012-05-02 (2.0)
;;    add setting for browse kill ring + add csv mode + svn mode + diff color
;; 2012-04-26 (1.9)
;;    update GNU Global to 6.2.2
;; 2012-04-23 (1.8)
;;    add undo tree mode to visualize undo/redo
;; 2012-04-16 (1.7)
;;    change some comments and working environment
;; 2012-03-30 (1.6)
;;    translate comments in english + google calendar
;; 2012-03-27 (1.5)
;;    add auto highlight symbol minor mode
;; 2012-03-26 (1.4)
;;    add outline minor mode
;; 2012-03-18 (1.3)
;;    add mode for RTRT script (.ptu)
;; 2012-03-03 (1.2)
;;    add selection of wich cedet is used
;; 2011-04-21 (1.1)
;;    add some test about ms windows for paths
;; 2010-10-13 (1.0)
;;    split for ecb parameter
;; 2006-10-13 (0.1)
;;    creation from scratch + doxymacs (no history since)


;;; Code:
;;
;;; DOXYMACS
;; need to configure and use it
;; REQUIREMENT: var     `section-mode-doxymacs'
(when section-mode-doxymacs (message "  2.1 Doxymacs...")
  (when (try-require 'doxymacs "    ")
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    (defvar doxymacs-doxygen-style "JavaDoc")
    (defun my-doxymacs-font-lock-hook ()
      (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
    (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook))
  (message "  2.1 Doxymacs... Done"))

;;
;;; IDO
;; REQUIREMENT: var     `section-mode-ido'
;; better 'switch buffers' (C-x C-b ou M-a) and 'open file' (C-x C-f)
;; erratic behavior with exotic filename
;; bug with module 'tramp' (not used)
(when section-mode-ido (message "  2.2 Ido...")
  (when (try-require 'ido "    ")
    (ido-mode t))
  (message "  2.2 Ido... Done"))

;;
;;; UNIQUIFY
;; REQUIREMENT: var     `section-mode-uniquify'
;; create unique buffer names with shared directory components)
(when section-mode-uniquify (message "  2.3 Uniquify...")
  (when (try-require 'uniquify "    ")
    (custom-set-variables
      '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))))
  (message "  2.3 Uniquify... Done"))

;;
;;; CEDET
;; REQUIREMENT: var     `section-mode-cedet'
;;              var     `clt-cedet-path'
(when section-mode-cedet
;; to remove warning:
;; Warning: cedet-called-interactively-p called with 0 arguments, but requires 1
  (setq byte-compile-warnings nil)
  ;; if the path is define use it to load cedet
  (if clt-cedet-path
    (progn
      (message "  2.4 CEDET bzr...")
      ;; REQUIREMENT:
      ;; need to remove `your-emacs-path/lisp/cedet'
      ;;                `your-emacs-path/lisp/speedbar.*'
      ;;                `your-emacs-path/lisp/emacs-lisp/eieio*'
      (defvar clt-cedet-bzr t)
      )
    (progn
      (message "  2.4 emacs included CEDET...")
      (defvar clt-cedet-bzr nil)
      )
    )
  ;; init the state of the loading of cedet
  (defvar clt-cedet-loaded nil)
  (if clt-cedet-bzr
    ;; load from the path clt-cedet-path
    (progn
      (if (load-file clt-cedet-path)
        (setq clt-cedet-loaded t)
        (message "    cedet was not loaded. Have you removed your-emacs-path/lisp/cedet/ your-emacs-path/lisp/speedbar.* and your-emacs-path/lisp/emacs-lisp/eieio* ?")
        )
      )
    ;; load from Emacs built-in cedet
    (progn
      (if (try-require 'cedet "    ")
        (setq clt-cedet-loaded t)
        (message "    cedet was not loaded. Have you GNU/Emacs 23.x ?")
        )
      )
    )
  ;; only if cedet can be loaded
  (when clt-cedet-loaded
    (cond
      ;; Magneti Marelli -------------------------------------------------------
      ((string= clt-working-environment "Magneti Marelli")
        (custom-set-variables
          ;; bin path of gnu global for cedet
          '(cedet-global-command "d:/cygwin/bin/global.exe")
          '(cedet-global-gtags-command "d:/cygwin/bin/gtags.exe"))
        ) ; Magneti Marelli

      ;; Alstom Transport ------------------------------------------------------
      ((string= clt-working-environment "Alstom Transport")
        (custom-set-variables
          ;; bin path of gnu global for cedet
          '(cedet-global-command "d:/cygwin/usr/local/bin/global.exe")
          '(cedet-global-gtags-command "d:/cygwin/usr/local/bin/gtags.exe"))
        ) ; Alstom Transport

      ) ; cond -----------------------------------------------------------------

    ;;
    ;;; SEMANTIC
    ;; REQUIREMENT:     var     `section-mode-cedet-semantic'
    ;; code source parser, etc
    (when section-mode-cedet-semantic (message "    2.4.1 Semantic...")
      (load-file (concat dotemacs-path "/dotemacs/mode-semantic.el"))
      (message "    2.4.1 Semantic... Done"))

    ;;
    ;;; ECB (Emacs Code Browser)
    ;; REQUIREMENT:     var     `section-mode-cedet-ecb'
    ;; transform Emacs interface to IDE
    (when section-mode-cedet-ecb (message "    2.4.2 ECB...")
      (load-file (concat dotemacs-path "/dotemacs/mode-ecb.el"))
      (message "    2.4.2 ECB... Done"))
    )
  (message "  2.4 CEDET... Done"))

;;
;;;    BATCH
;; REQUIREMENT: var     `section-mode-batch'
;; syntax color for .bat script for MS Windows
(when section-mode-batch (message "  2.5 Batch Windows...")
  (autoload 'batch-mode "batch-mode" "Load batch-mode")
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
  (message "  2.5 Batch Windows... Done"))

;;
;;; VISUAL BASIC
;; REQUIREMENT: var     `section-mode-vb'
;; syntax color for sources in VB and VBA
(when section-mode-vb (message "  2.6 Visual Basic...")
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                    visual-basic-mode)) auto-mode-alist))
  (message "  2.6 Visual Basic... Done"))

;;
;;; WINDOW NUMBERING
;; REQUIREMENT: var     `section-mode-window-numbering'
;; shortcut to go to window
;; (give a number at each window so you can easily switch between window with
;; shortcut `M-<number>')
(when section-mode-window-numbering (message "  2.7 Windows Numbering...")
  (when (try-require 'window-numbering "    ")
    (window-numbering-mode 1))
  (message "  2.7 Windows Numbering... Done"))

;;
;;; C
;; REQUIREMENT: var     `section-mode-c'
(when section-mode-c (message "  2.8 C...")
  ;; new types
  (defvar c-font-lock-extra-types
    (list "ubyte" "ushort" "ulong" "ulonglong" "sbyte" "sshort" "slong" "slonglong"))

  ;;;; TRY
  ;;;; define extra C types to font-lock
  ;;(setq c-font-lock-extra-types
  ;;  (append
  ;;    '("CHAR" "BOOL" "BYTE" "SOCKET" "boolean" "UINT" "UINT16"
  ;;       "UINT32" "ULONG" "FLOAT" "INT" "INT16" "INT32""uint" "ulong" "string"
  ;;       "BOOLEAN" "\\sw+_T")
  ;;    c-font-lock-extra-types))

  ;;; CWARN
  ;; REQUIREMENT:       var     `section-mode-c-cwarn'
  ;; show small warning in code source
  ;; (ex: set in test, semi colon after test...)
  (when section-mode-c-cwarn (message "    2.8.1 CWarn...")
    (cwarn-mode t)
    (message "    2.8.1 CWarn... Done"))

  ;;; DATA DEBUG
  ;; REQUIREMENT:       var     `section-mode-c-data-debug'
  ;; ??
  ;; see sortcut `M-:'
  (when section-mode-c-data-debug (message "2.8.2 C Data Debug...")
    (when (try-require 'data-debug "    "))
    (message "2.8.2 C Data Debug... Done"))

  (autoload 'rebox-comment "rebox" nil t)
  (autoload 'rebox-region "rebox" nil t)
  (setq rebox-default-style 245)

  ;;; Compil mode
  (setq compile-command "ccm objectmake")
  (message "  2.8 C... Done"))

;;
;;; ICOMPLETION
;; REQUIREMENT: var     `section-mode-icompletion'
(when section-mode-icompletion (message "  2.9 Icompletion...")
  ;; Completion in minibuffer
  (icomplete-mode t)
  (message "  2.9 Icompletion... Done"))

;;
;;; YASNIPPET
;; REQUIREMENT: var     `section-mode-yasnippet'
(when section-mode-yasnippet (message "  2.10 Yasnippet...")
;; enable snippet (see `../emacs.el' for definition)
  (add-to-list 'load-path (concat dotemacs-path "/plugins/yasnippet-0.6.1c"))
  (when (try-require 'yasnippet "    ") ; not yasnippet-bundle
    (setq yas/root-directory (concat dotemacs-path "/plugins/yasnippet-0.6.1c/snippets"))
    (yas/load-directory yas/root-directory))
  (yas/global-mode)
  (message "  2.10 Yasnippet... Done"))

;;
;;; BROWSE KILL RING
;; REQUIREMENT: var     `section-mode-browse-kill-ring'
(when section-mode-browse-kill-ring (message "  2.11 Browse Kill-Ring...")
  (when (try-require 'browse-kill-ring "    ")
    ;; all settings from Fabrice Niessen
    ;; string separating entries in the `separated' style
    (setq browse-kill-ring-separator
          "\n--separator------------------------------")

    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)

    ;; face in which to highlight the `browse-kill-ring-separator'
;    (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
                                        ; slate gray
;    (setq browse-kill-ring-separator-face 'separator-face)

    ;; use `M-y' to invoke `browse-kill-ring'
    (browse-kill-ring-default-keybindings))
  (message "  2.11 Browse Kill-Ring... Done"))

;;
;;; MAGNETI MARELLI
;; REQUIREMENT: var     `section-mode-mm'
(when section-mode-mm (message "  2.12 Magneti Marelli...")
  ;;; EOL
  ;; REQUIREMENT:       var     `section-mode-mm-eol'
  (when section-mode-mm-eol (message "    2.12.1 EOL...")
    (load-file (concat dotemacs-path "/plugins/mm-eol.el"))
    (message "    2.12.1 EOL... Done"))

  ;;; CAN DBC
  ;; REQUIREMENT:       var     `section-mode-mm-dbc'
  (when section-mode-mm-dbc (message "    2.12.2 CAN Dbc...")
    (load-file (concat dotemacs-path "/plugins/mm-dbc.el"))
    (message "    2.12.2 CAN Dbc... Done"))

  ;;; CCM DIFF
  ;; REQUIREMENT:       var     `section-mode-mm-diff'
  (when section-mode-mm-diff (message "    2.12.3 Synergy Diff...")
    (load-file (concat dotemacs-path "/plugins/mm-diff.el"))
    (message "    2.12.3 Synergy Diff... Done"))
  (message "  2.12 Magneti Marelli... Done"))

;;
;;; DIRED+
;; REQUIREMENT: var     `section-mode-dired-plus'
(when section-mode-dired-plus (message "  2.13 Dired+...")
  (try-require 'dired+ "    ")
  (message "  2.13 Dired+... Done"))

;;
;;; GNU/GLOBAL
;; REQUIREMENT: var     `section-mode-gnu-global'
(when section-mode-gnu-global (message "  2.14 GNU/Global...")
  (when (try-require 'gtags "    ")
    (autoload 'gtags-mode "gtags" "" t)
    (defun gtags-c-mode ()
      (gtags-mode 1)
      (setq gtags-select-buffer-single t)
      )
    (add-hook 'c-mode-common-hook 'gtags-c-mode)
    )
  (message "  2.14 GNU/Global... Done"))

;;
;;; EPROJECT (grischka) ; never used
;; REQUIREMENT: var     `section-mode-eproject'
(when section-mode-eproject (message "  2.15 Eproject...")
  (load-file (concat dotemacs-path "/plugins/eproject.el"))
  ;;(when (try-require 'eproject))
  (message "  2.15 Eproject... Done"))

;;
;;; RTRT SCRIPT
;; REQUIREMENT: var     `section-mode-rtrt-script'
(when section-mode-rtrt-script (message "  2.16 RTRT script...")
  (load-file (concat dotemacs-path "/plugins/rtrt-script.elc"))
  (message "  2.16 RTRT script... Done"))

;;
;;; VC CLEARCASE
;; REQUIREMENT: var     `section-mode-vc-clearcase'
;;              var     `clt-working-environment' "Alstom Transport"
(when section-mode-vc-clearcase (message "  2.17 VC ClearCase...")
  (add-to-list 'load-path  (concat dotemacs-path "/plugins/vc-clearcase-3.6"))
  (setq load-path (append load-path '(concat dotemacs-path "/plugins/vc-clearcase-3.6")))
  (load "vc-clearcase-auto")
  (cond
    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (custom-set-variables
        '(clearcase-checkout-comment-type (quote normal))
        '(clearcase-use-external-diff t)
        '(clearcase-vtree-program "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/clearvtree.exe")
        '(cleartool-program "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/cleartool.exe")
        )
      ) ; Alstom Transport
    ) ; cond -------------------------------------------------------------------
  (message "  2.17 VC ClearCase... Done"))

;;
;;; CLEARCASE
;; REQUIREMENT: var     `section-mode-clearcase'
(when section-mode-clearcase (message "  2.18 ClearCase...")
  (load "clearcase")
  (message "  2.18 ClearCase... Done"))

;;
;;; AUTOHOTKEY
;; REQUIREMENT: var     `section-mode-autohotkey'
(when section-mode-autohotkey (message "  2.19 AutoHotKey...")
  ;; this folder doesn't exist anymore ; os the mode doesn't work
  (setq ahk-syntax-directory "PATHTO/AutoHotkey/Extras/Editors/Syntax/")
  (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
  (autoload 'ahk-mode-fix "ahk-mode")
  (message "  2.19 AutoHotKey... Done"))

;;
;;; OUTLINE
;; REQUIREMENT: var     `section-mode-outline'
(when section-mode-outline (message "  2.20 Outline minor mode...")
  ;; to manually hide some block in code source
  (outline-minor-mode 1)
  (message "  2.20 Outline minor mode... Done"))

;;
;;; AUTO HIGHLIGHT SYMBOL
;; REQUIREMENT: var     `section-mode-auto-highlight-symbol'
(when section-mode-auto-highlight-symbol (message "  2.21 Auto highlight symbol minor mode...")
  ;; after some idle time the symbol at point will be highlighted in display area
  (require 'auto-highlight-symbol)
  ;; active the mode
  (global-auto-highlight-symbol-mode t)
  (custom-set-variables
    ;; do not ignore case
    '(ahs-case-fold-search nil)
    ;; increase idle time to display highlight
    '(ahs-idle-interval 2.2)
    )
  (message "  2.21 Auto highlight symbol minor mode... Done"))

;;
;;; GOOGLE CALENDAR
;; REQUIREMENT: var     `section-mode-google-calendar'
(when section-mode-google-calendar (message "  2.22 Google Calendar...")
  ;; can import google calendar in Emacs calendar
  (when (try-require 'icalendar)
    (when (try-require 'google-calendar)
      (setq google-calendar-user           "personne146@gmail.com")
      (setq google-calendar-code-directory (concat dotemacs-path "/plugins/google"))
      (setq google-calendar-directory      "~/tmp")
      (setq google-calendar-url            "http://www.google.com/calendar/ical/personne146%40gmail.com/private-9d0820c331c8b9b271a921d00fe017aa/basic.ics")
      (setq google-calendar-auto-update    t)
      (google-calendar-download)
      ))
  (message "  2.22 Google Calendar... Done"))

;;
;;; FILL COLUMN INDICATOR
;; REQUIREMENT: var     `section-mode-fill-column-indicator'
(when section-mode-fill-column-indicator (message "  2.23 Fill Column Indicator...")
  ;; can import google calendar in Emacs calendar
  (when (try-require 'fill-column-indicator)
    ;; width of line
    (setq fci-rule-width 1)
    ;; color of line
    (setq fci-rule-color "grey15")
    ;; only in C mode
    (add-hook 'c-mode-hook 'fci-mode)
    (add-hook 'c++-mode-hook 'fci-mode)
    ;;;; to show for all files
    ;;(add-hook 'after-change-major-mode-hook 'fci-mode)
    )
  (message "  2.23 Fill Column Indicator... Done"))

;;
;;; MUSE
;; REQUIREMENT: var     `section-mode-muse'
(when section-mode-muse (message "  2.24 Muse...")
  (add-to-list 'load-path  (concat dotemacs-path "/plugins/muse-3.20/bin"))
  (setq load-path (cons (expand-file-name (concat dotemacs-path "/plugins/muse-3.20/bin")) load-path))

  (try-require 'muse-mode "    ")     ; load authoring mode

  (try-require 'muse-html "    ")     ; load publishing styles I use
  (try-require 'muse-latex "    ")
  (try-require 'muse-texinfo "    ")
  (try-require 'muse-docbook "    ")

  (try-require 'muse-project "    ")  ; publish files in projects
  (message "  2.24 Muse... Done"))

;;
;;; UNDO TREE
;; REQUIREMENT: var     `section-mode-undo-tree'
(when section-mode-undo-tree (message "  2.25 Undo Tree...")
  (when (try-require 'undo-tree "    ")
    ;; If you want to replace the standard Emacs' undo system with the
    ;; `undo-tree-mode' system in all buffers, you can enable it globally by
    ;; adding:
    ;;
    (global-undo-tree-mode t))
  (message "  2.25 Undo Tree... Done"))

;;
;;; CSV
;; REQUIREMENT: var     `section-mode-csv'
(when section-mode-csv (message "  2.26 CSV...")
  (when (try-require 'csv-mode "    ")
    ;; field separators: a list of *single-character* strings
    (setq csv-separators '("," ";")))
  (message "  2.26 CSV... Done"))

;;
;;; SUBVERSION
;; REQUIREMENT: var     `section-mode-subversion'
(when section-mode-subversion (message "  2.27 Subversion 1.7...")
  (try-require 'vc-svn17 "    ")
  (message "  2.27 Subversion 1.7... Done"))

;;
;;; DIFF COLOR
;; REQUIREMENT: var     `section-mode-diff-color'
(when section-mode-diff-color (message "  2.28 Diff Color...")
  (try-require 'diff-mode- "    ")
  (message "  2.28 Diff Color... Done"))

;;
;;; DIRED SORT
;; REQUIREMENT: var     `section-mode-dired-sort'
(when section-mode-dired-sort (message "  2.29 Dired Sort...")
  (try-require 'dired-sort-menu)
  (custom-set-variables
    ;; set a profile of sorting
    '(dired-sort-menu-saved-config
       (quote (
                (dired-actual-switches . "-alh")
                (ls-lisp-ignore-case . t)
                (ls-lisp-dirs-first . t))))
    ;; set this profile by default
    '(ls-lisp-dirs-first t)
    '(ls-lisp-ignore-case t)
    )
  (message "  2.29 Dired Sort... Done"))

;;
;;; ORG MODE
;; REQUIREMENT: var     `section-mode-org-mode'
(when section-mode-org-mode (message "  2.30 Org Mode...")
  ;; The following lines are always needed.  Choose your own keys.
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (message "  2.30 Org Mode... Done"))

(custom-set-variables
;;
;;; HIDE IFDEF
  '(hide-ifdef-initially t)
  '(hide-ifdef-lines nil)
  '(hide-ifdef-shadow t)
  '(hs-hide-comments-when-hiding-all nil)
  )

;; TRY
(when 0
  ;; it add a small round in modeline where color give status of svn
  ;; + show all file status of a directory
  (try-require 'psvn)
)
;; need to try
;(autoload 'ifdef:ifdef-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:ifndef-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:ifdef-else-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:if-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:if-else-region "ifdef" "ifdef your code" t)

;;; mode.el ends here
