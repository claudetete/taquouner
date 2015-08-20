;;; mode.el --- a config file for all mode settings

;; Copyright (c) 2006-2014 Claude Tete
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
;; Version: 4.9
;; Created: October 2006
;; Last-Updated: March 2014

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-mode'
;;              var     `section-external-directory'

;;; Change Log:
;; 2014-03-26 (4.9)
;;    add synergy-web mode + clean last added mode
;; 2013-09-10 (4.8)
;;    add magit mode
;; 2013-05-30 (4.7)
;;    add ediff mode settings
;; 2013-05-23 (4.6)
;;    add condition for powerline modeline + add rainbow mode
;; 2013-05-07 (4.5)
;;    close compile window when quit clearcase config spec mode + use fork of
;;    powerline mode + close compile window when quit helm + add yascroll and
;;    smart-forward mode
;; 2013-04-16 (4.4)
;;    fix bug with powerline and clearcase not used
;; 2013-04-12 (4.3)
;;    fix bug helm with browse kill ring + use autoload when it's possible +
;;    clean up clearcase mode
;; 2013-04-10 (4.2)
;;    dired buffer is reused + outline in rtrt + helm mode + clean up
;; 2013-03-26 (4.1)
;;    update powerline mode
;; 2013-02-05 (4.0)
;;    add auctex mode
;; 2012-12-27 (3.9)
;;    update dot emacs path + update color setting for powerline + fix bug with
;;    dired-details + add diredful and ps2pdf mode
;; 2012-11-29 (3.8)
;;    add ace jump mode
;; 2012-11-26 (3.7)
;;    add ack mode, 3 modes exist and do not work immediately, need to patch
;;    ack-standalone (put in .emacs.d/plugins/) and try to patch/complete
;;    ack-emacs mode (the simplest)
;; 2012-10-26 (3.6)
;;    add FastNav and MRU Yank mode
;; 2012-10-18 (3.5)
;;    fix bug without clearcase mode and with powerline + add dired lis and
;;    nxhtml mode
;; 2012-08-01 (3.4)
;;    modify settings for csv mode
;; 2012-07-19 (3.3)
;;    add fold dwim mode
;; 2012-07-11 (3.2)
;;    add calfw (calendar) + dired details + smart tab
;; 2012-07-09 (3.1)
;;    move extern file in mode + setting of browse kill ring + new autohotkey
;;    mode + dired and isearch setting + rainbow delimiters
;; 2012-06-19 (3.0)
;;    add option for powerline
;; 2012-06-13 (2.9)
;;    add powerline, sml modeline, diminish and nyan mode
;; 2012-06-12 (2.8)
;;    move loading of project from mode-semantic.el to here
;; 2012-06-06 (2.7)
;;    add psvn mode + dired settings + clean try
;; 2012-06-05 (2.6)
;;    remove all profile dependances + use require to nicer messages
;; 2012-05-29 (2.5)
;;    remove custom compile command (put in EDE project file see in
;;    `.emacs.d/myproject.ede.el')
;; 2012-05-25 (2.4)
;;    modify path for global.exe and gtags.exe for cedet and add default working
;;    environment + add isearch+ mode
;; 2012-05-23 (2.3)
;;    add two new style for muse and slides
;; 2012-05-10 (2.2)
;;    add org mode (not used)
;; 2012-05-04 (2.1)
;;    add Dired sort mode
;; 2012-05-02 (2.0)
;;    add setting for browse kill ring + add CSV mode + SVN mode + diff color
;; 2012-04-26 (1.9)
;;    update GNU Global to 6.2.2
;; 2012-04-23 (1.8)
;;    add undo tree mode to visualize undo/redo
;; 2012-04-16 (1.7)
;;    change some comments and working environment
;; 2012-03-30 (1.6)
;;    translate comments in English + Google calendar
;; 2012-03-27 (1.5)
;;    add auto highlight symbol minor mode
;; 2012-03-26 (1.4)
;;    add outline minor mode
;; 2012-03-18 (1.3)
;;    add mode for RTRT script (.ptu)
;; 2012-03-03 (1.2)
;;    add selection of which cedet is used
;; 2011-04-21 (1.1)
;;    add some test about ms windows for paths
;; 2010-10-13 (1.0)
;;    split for ecb parameter
;; 2006-10-13 (0.1)
;;    creation from scratch + doxymacs (no history since)


;;; Code:

;;; DIRECTORY
(when section-mode-directory (message "  2.1 Load Directory...")
  ;; path to load mode
  (add-to-list 'load-path (file-name-as-directory dotemacs-path))
  (add-to-list 'load-path  (concat (file-name-as-directory dotemacs-path) "plugins"))
  (message "  2.1 Load Directory... Done"))

;;
;;; VECTRA
(when section-mode-vectra (message "  2.2 Vectra...")
;; Vectra man and doc (the rest is not very useful)
  (try-require 'vectra "    ")
  (message "  2.2 Vectra... Done"))

;;
;;; HOME/END
(when section-mode-home-end (message "  2.3 Home/End...")
  ;; to add features to home/end key (two push will get you at the end/start
  ;; of display) (three push will get you at the end/start of buffer)
  (try-require 'pc-keys "    ")
  (message "  2.3 Home/End... Done"))

;;
;;; DOXYMACS
;; need to configure and use it
(when section-mode-doxymacs (message "  2.4 Doxymacs...")
  (when (try-require 'doxymacs "    ")
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    (defvar doxymacs-doxygen-style "JavaDoc")
    (defun my-doxymacs-font-lock-hook ()
      (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
    (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook))
  (message "  2.4 Doxymacs... Done"))

;;
;;; IDO
;; better 'switch buffers' (C-x C-b or M-a) and 'open file' (C-x C-f)
;; erratic behavior with exotic filename
;; bug with module 'tramp' (not used)
(when section-mode-ido (message "  2.5 Ido...")
  (when (try-require 'ido "    ")
    (ido-mode t))
  (message "  2.5 Ido... Done"))

;;
;;; UNIQUIFY
;; create unique buffer names with shared directory components)
(when section-mode-uniquify (message "  2.6 Uniquify...")
  (when (try-require 'uniquify "    ")
    (custom-set-variables
      '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))))
  (message "  2.6 Uniquify... Done"))

;;
;;; CEDET
;; REQUIREMENT: var     `section-mode-cedet'
;;              var     `profile-cedet-path'
(when section-mode-cedet
;; to remove warning:
;; Warning: cedet-called-interactively-p called with 0 arguments, but requires 1
  (setq byte-compile-warnings nil)
  ;; if the path is define use it to load cedet
  (if profile-cedet-path
    (progn
      (message "  2.7 CEDET bzr...")
      ;; REQUIREMENT:
      ;; need to remove `your-emacs-path/lisp/cedet'
      ;;                `your-emacs-path/lisp/speedbar.*'
      ;;                `your-emacs-path/lisp/emacs-lisp/eieio*'
      (defvar section-mode-cedet-bzr t)
      )
    (progn
      (message "  2.7 emacs included CEDET...")
      (defvar section-mode-cedet-bzr nil)
      )
    )
  ;; init the state of the loading of cedet
  (defvar section-mode-cedet-loaded nil)
  (if section-mode-cedet-bzr
    ;; load from the path clt-cedet-path
    (progn
      (if (load-file profile-cedet-path)
        (setq section-mode-cedet-loaded t)
        (message "    cedet was not loaded. Have you removed your-emacs-path/lisp/cedet/ your-emacs-path/lisp/speedbar.* and your-emacs-path/lisp/emacs-lisp/eieio* ?")
        )
      )
    ;; load from Emacs built-in cedet
    (progn
      (if (try-require 'cedet "    ")
        (setq section-mode-cedet-loaded t)
        (message "    cedet was not loaded. Have you GNU/Emacs 23.4 or 24.x ?")
        )
      )
    )
  ;; only if cedet can be loaded
  (when section-mode-cedet-loaded
    ;; bin path of gnu global for cedet
    (setq cedet-global-command profile-gnu-global)
    (setq cedet-global-gtags-command profile-gnu-global-gtags)

    ;;
    ;;; SEMANTIC
    ;; code source parser, etc
    (when section-mode-cedet-semantic (message "    2.7.1 Semantic...")
      (try-require 'mode-semantic "      ")
      (message "    2.7.1 Semantic... Done"))

    ;;
    ;;; ECB (Emacs Code Browser)
    ;; transform Emacs interface to IDE
    (when section-mode-cedet-ecb (message "    2.7.2 ECB...")
      (try-require 'mode-ecb "      ")
      (message "    2.7.2 ECB... Done"))

    ;; load the different projects
    (try-require 'project "      ")
    )
  (message "  2.7 CEDET... Done"))

;;
;;;    BATCH
;; syntax color for .bat script for MS Windows
(when section-mode-batch (message "  2.8 Batch Windows...")
  (autoload 'batch-mode "batch-mode" "Load batch-mode")
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
  (message "  2.8 Batch Windows... Done"))

;;
;;; VISUAL BASIC
;; syntax color for sources in VB and VBA
(when section-mode-vb (message "  2.9 Visual Basic...")
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                    visual-basic-mode)) auto-mode-alist))
  (message "  2.9 Visual Basic... Done"))

;;
;;; WINDOW NUMBERING
;; shortcut to go to window
;; (give a number at each window so you can easily switch between window with
;; shortcut `M-<number>')
(when section-mode-window-numbering (message "  2.10 Windows Numbering...")
  (when (try-require 'window-numbering "    ")
    (window-numbering-mode 1))
  (message "  2.10 Windows Numbering... Done"))

;;
;;; C must be put in languages,el
(when section-mode-c (message "  2.11 C...")
  ;;; CWARN
  ;; show small warning in code source
  ;; (ex: set in test, semi colon after test...)
  (when section-mode-c-cwarn (message "    2.11.1 CWarn...")
    (cwarn-mode t)
    (message "    2.11.1 CWarn... Done"))

  ;;; DATA DEBUG
  ;; ??
  ;; see sortcut `M-:'
  (when section-mode-c-data-debug (message "2.11.2 C Data Debug...")
    (when (try-require 'data-debug "    "))
    (message "2.11.2 C Data Debug... Done"))

  ;;(autoload 'rebox-comment "rebox" nil t)
  ;;(autoload 'rebox-region "rebox" nil t)
  ;;(setq rebox-default-style 245)
  (message "  2.11 C... Done"))

;;
;;; ICOMPLETION
(when section-mode-icompletion (message "  2.12 Icompletion...")
  ;; Completion in Minibuffer
  (icomplete-mode t)
  (message "  2.12 Icompletion... Done"))

;;
;;; YASNIPPET
(when section-mode-yasnippet (message "  2.13 Yasnippet...")
;; enable snippet (see `../emacs.el' for definition)
  (add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "plugins/yasnippet-0.6.1c"))
  (when (try-require 'yasnippet "    ") ; not yasnippet-bundle
    (setq yas/root-directory (concat (file-name-as-directory dotemacs-path) "plugins/yasnippet-0.6.1c/snippets"))
    (yas/load-directory yas/root-directory))
  (yas/global-mode)
  (message "  2.13 Yasnippet... Done"))

;;
;;; BROWSE KILL RING
(when section-mode-browse-kill-ring (message "  2.14 Browse Kill-Ring...")
  (when (try-require 'autoload-browse-kill-ring "    ")
    ;; all settings from Fabrice Niessen
    ;; string separating entries in the `separated' style
    (setq browse-kill-ring-separator
      "\n--separator------------------------------")
    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)
    (when (not section-mode-helm-kill-ring)
      ;; use `M-y' to invoke `browse-kill-ring'
      (browse-kill-ring-default-keybindings))
    ;; do not show duplicate in list
    (setq browse-kill-ring-display-duplicates nil)
    ;; do not add duplicate in kill ring
    (setq browse-kill-ring-no-duplicates t)
    )
  (message "  2.14 Browse Kill-Ring... Done"))

;;
;;; MAGNETI MARELLI
(when section-mode-mm (message "  2.15 Magneti Marelli...")
  ;;; EOL
  (when section-mode-mm-eol (message "    2.15.1 EOL...")
    (try-require 'mm-eol "      ")
    (message "    2.15.1 EOL... Done"))
  ;;; CAN DBC
  (when section-mode-mm-dbc (message "    2.15.2 CAN Dbc...")
    (try-require 'mm-dbc "      ")
    (message "    2.15.2 CAN Dbc... Done"))
  ;;; CCM DIFF
  (when section-mode-mm-diff (message "    2.15.3 MM Diff...")
    (try-require 'mm-diff "      ")
    (message "    2.15.3 MM Diff... Done"))
  (message "  2.15 Magneti Marelli... Done"))

;;
;;; DIRED+
(when section-mode-dired-plus (message "  2.16 Dired+...")
  (when (try-require 'autoload-dired+ "    ")
    ;; to have only one dired buffer by dired instance
    (toggle-diredp-find-file-reuse-dir t))
  (message "  2.16 Dired+... Done"))

;;
;;; GNU/GLOBAL
(when section-mode-gnu-global (message "  2.17 GNU/Global...")
  (when (try-require 'autoload-gtags "    ")
    (autoload 'gtags-mode "gtags" "" t)
    (defun gtags-c-mode ()
      (gtags-mode 1)
      (setq gtags-select-buffer-single t)
      )
    (gtags-mode t) ; only for diminish mode
    (add-hook 'c-mode-common-hook 'gtags-c-mode)
    (add-hook 'emacs-lisp-mode-hook 'gtags-c-mode)
    )
  (message "  2.17 GNU/Global... Done"))

;;
;;; EPROJECT (grischka) ; never used
(when section-mode-eproject (message "  2.18 Eproject...")
  (try-require 'eproject "    ")
  ;;(when (try-require 'eproject))
  (message "  2.18 Eproject... Done"))

;;
;;; RTRT SCRIPT
(when section-mode-rtrt-script (message "  2.19 RTRT script...")
  (when (try-require 'rtrt-ptu "    ")
    (when section-mode-outline
      (outline-minor-mode 1)))
  (message "  2.19 RTRT script... Done"))

;;
;;; VC CLEARCASE ; do not work
(when section-mode-vc-clearcase (message "  2.20 VC ClearCase...")
  (add-to-list 'load-path  (concat (file-name-as-directory dotemacs-path) "plugins/vc-clearcase-3.6"))
  (try-require 'vc-clearcase-auto "    ")
  (custom-set-variables
    '(clearcase-checkout-comment-type (quote normal))
    '(clearcase-use-external-diff t)
    '(clearcase-vtree-program profile-clearcase-vtree)
    '(cleartool-program profile-cleartool)
    )
  (message "  2.20 VC ClearCase... Done"))

;;
;;; CLEARCASE
(setq clearcase-mode nil)
(when section-mode-clearcase (message "  2.21 ClearCase...")
  ;; toggle compile window when quit config spec mode
  (when section-mode-cedet-ecb
    (add-hook 'clearcase-config-spec-quit-hook 'ecb-toggle-compile))
  (when section-mode-clearcase-el
    (try-require 'clearcase "    "))
  (message "  2.21 ClearCase... Done"))

;;
;;; AUTOHOTKEY
(when section-mode-autohotkey (message "  2.22 AutoHotKey...")
  (try-require 'xahk-mode "    ")
  (add-to-list 'auto-mode-alist '("\\.ahk$" . xahk-mode))
  (message "  2.22 AutoHotKey... Done"))

;;
;;; OUTLINE
(when section-mode-outline (message "  2.23 Outline minor mode...")
  ;; to manually hide some block in code source
  (outline-minor-mode 1)
  (message "  2.23 Outline minor mode... Done"))

;;
;;; AUTO HIGHLIGHT SYMBOL
(when section-mode-auto-highlight-symbol (message "  2.24 Auto highlight symbol minor mode...")
  ;; after some idle time the symbol at point will be highlighted in display area
  (when (try-require 'autoload-auto-highlight-symbol "    ")
    ;; active the mode
    (global-auto-highlight-symbol-mode t)
    (custom-set-variables
      ;; do not ignore case
      '(ahs-case-fold-search nil)
      ;; increase idle time to display highlight
      '(ahs-idle-interval 2.2)
      )
    )
  (message "  2.24 Auto highlight symbol minor mode... Done"))

;;
;;; GOOGLE CALENDAR
(when section-mode-google-calendar (message "  2.25 Google Calendar...")
  ;; can import google calendar in Emacs calendar
  (when (try-require 'icalendar "    ")
    (when (try-require 'google-calendar "    ")
      (setq google-calendar-user           profile-google-calendar-user)
      (setq google-calendar-code-directory profile-google-calendar-src)
      (setq google-calendar-directory      profile-google-calendar-directory)
      (setq google-calendar-url            profile-google-calendar-url)
      (setq google-calendar-auto-update    t)
      (google-calendar-download)
      ))
  (message "  2.25 Google Calendar... Done"))

;;
;;; FILL COLUMN INDICATOR
(when section-mode-fill-column-indicator (message "  2.26 Fill Column Indicator...")
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
  (message "  2.26 Fill Column Indicator... Done"))

;;
;;; MUSE
(when section-mode-muse (message "  2.27 Muse...")
  (add-to-list 'load-path  (concat (file-name-as-directory dotemacs-path) "plugins/muse-3.20/bin"))

  (try-require 'muse-mode "    ")     ; load authoring mode

  (try-require 'muse-html "    ")     ; load publishing styles I use
  (try-require 'muse-latex "    ")

  (muse-derive-style "my-slides-pdf" "slides-pdf"
    :header (concat (file-name-as-directory dotemacs-path) "plugins/themes/muse/header.tex")
    :footer  (concat (file-name-as-directory dotemacs-path) "plugins/themes/muse/footer.tex")
    )

  (muse-derive-style "my-slides" "slides"
    :header (concat (file-name-as-directory dotemacs-path) "plugins/themes/muse/header.tex")
    :footer  (concat (file-name-as-directory dotemacs-path) "plugins/themes/muse/footer.tex")
    )

  (try-require 'muse-project "    ")  ; publish files in projects
  (message "  2.27 Muse... Done"))

;;
;;; UNDO TREE
(when section-mode-undo-tree (message "  2.28 Undo Tree...")
  (when (try-require 'autoload-undo-tree "    ")
    ;; If you want to replace the standard Emacs' undo system with the
    ;; `undo-tree-mode' system in all buffers, you can enable it globally by
    ;; adding:
    ;;
    (global-undo-tree-mode t))
  (message "  2.28 Undo Tree... Done"))

;;
;;; CSV
(when section-mode-csv (message "  2.29 CSV...")
  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t)
  (custom-set-variables
    '(csv-separators (quote (";"))))
  (message "  2.29 CSV... Done"))

;;
;;; SUBVERSION
(when section-mode-subversion (message "  2.30 Subversion 1.7...")
  (try-require 'vc-svn17 "    ")
  (message "  2.30 Subversion 1.7... Done"))

;;
;;; DIFF COLOR
(when section-mode-diff-color (message "  2.31 Diff Color...")
  (try-require 'autoload-diff-mode- "    ")
  (message "  2.31 Diff Color... Done"))

;;
;;; DIRED SORT
(when section-mode-dired-sort (message "  2.32 Dired Sort...")
  (when (try-require 'autoload-dired-sort-menu "    ")
    (custom-set-variables
      '(dired-recursive-copies t)
      '(dired-recursive-deletes t)
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
    )
  (message "  2.32 Dired Sort... Done"))

;;
;;; ORG MODE
(when section-mode-org-mode (message "  2.33 Org Mode...")
  ;; The following lines are always needed.  Choose your own keys.
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (message "  2.33 Org Mode... Done"))

;;
;;; ISEARCH+
(when section-mode-isearch+ (message "  2.34 Isearch+...")
  (eval-after-load "isearch" '(try-require 'autoload-isearch+))
  (message "  2.34 Isearch+... Done"))

;;
;;; PSVN
;; it add a small round in modeline where color give status of SVN
;; + show all file status of a directory
(when section-mode-psvn (message "  2.35 PSvn...")
  (try-require 'autoload-psvn "    ")
  (message "  2.35 PSvn... Done"))

;;
;;; POWERLINE
(when section-mode-powerline (message "  2.36 Powerline...")
  (when running-on-emacs-23
    ;; in Emacs 23 it was not define
    (defun get-scroll-bar-mode () scroll-bar-mode)
    (defsetf get-scroll-bar-mode set-scroll-bar-mode))
  ;; use new powerline mode
  ;; see
  (when (try-require 'powerline "    ")
    (defun powerline-my-theme ()
      "Setup a mode-line."
      (interactive)
      (setq-default mode-line-format
        '("%e"
           (:eval
             (let* ((active (powerline-selected-window-active))
                     ;; face for right and left
                     (mode-line (if active 'mode-line 'mode-line-inactive))
                     ;; face for between right and left and middle
                     (face-between (if active 'powerline-active1
                                     'powerline-inactive1))
                     ;; face for middle
                     (face-middle (if active 'powerline-active2
                                    'powerline-inactive2))
                     ;; face for highlight
                     (face-warning 'font-lock-warning-face)
                     (separator-left
                       (intern (format "powerline-%s-%s"
                                 powerline-default-separator
                                 (car powerline-default-separator-dir))))
                     (separator-right
                       (intern (format "powerline-%s-%s"
                                 powerline-default-separator
                                 (cdr powerline-default-separator-dir))))
                     (lhs (list
                            ;;
                            ;; LEFT
                            ;; display [RO] when visited a read-only file
                            (when (and buffer-read-only buffer-file-name)
                              (powerline-raw "[RO]" face-warning))
                            ;; encoding and eol indicator
                            (powerline-raw mode-line-mule-info nil 'l)
                            ;; buffername
                            (powerline-buffer-id nil 'l)
                            ;; display * at end of buffer name when buffer was modified
                            (when (and (buffer-modified-p) buffer-file-name)
                              (powerline-raw "*" face-warning 'l))

                            ;; first separator
                            (powerline-raw " ")
                            (funcall separator-left mode-line face-between)

                            ;;
                            ;; LEFT MIDDLE
                            ;; major mode
                            (powerline-major-mode face-between 'l)
                            ;; process
                            (powerline-process face-between)
                            ;; minor mode
                            (powerline-minor-modes face-between 'l)
                            ;; narrow mode
                            (powerline-narrow face-between 'l)

                            ;; second separator
                            (powerline-raw " " face-between)
                            (funcall separator-left face-between face-middle)

                            ;;
                            ;; MIDDLE
                            ;; version control
                            (powerline-vc face-middle 'r)))
                     (rhs (list

                            ;; third separator
                            (powerline-raw global-mode-string face-middle 'r)
                            (funcall separator-right face-middle face-between)

                            ;;
                            ;; RIGHT MIDDLE
                            ;; line number
                            (powerline-raw "%2l" face-between 'l)
                            ;; :
                            (powerline-raw ":" face-between 'l)
                            ;; column number
                            (powerline-raw "%2c" face-between 'r)

                            ;; fourth separator
                            (funcall separator-right face-between mode-line)
                            (powerline-raw " ")

                            ;;
                            ;; RIGHT
                            ;; position indicator
                            (powerline-raw "%6p" nil 'r)
                            )))
               ;;(message "%s %s" separator-left (funcall 'powerline-wave-left mode-line face1))
               (concat
                 (powerline-render lhs)
                 (powerline-fill face-middle (powerline-width rhs))
                 (powerline-render rhs)))))))
    ;; set arrow fade as separator
    (setq powerline-default-separator 'arrow-fade)
    (powerline-my-theme)
    )
  (message "  2.36 Powerline... Done"))

;;
;;; NYAN
(when section-mode-nyan (message "  2.37 Nyan...")
  (when (try-require 'autoload-nyan-mode "    ")
    ;; start nyan mode
    (nyan-mode t)

    ;; to have wave in rainbow
    (setq nyan-wavy-trail 1)

    ;; to have animation
    (nyan-start-animation)
    )
  (message "  2.37 Nyan... Done"))

;;
;;; SML
(when section-mode-sml (message "  2.38 sml modeline...")
  (when (try-require 'autoload-sml-modeline "    ")
    (custom-set-variables
      '(sml-modeline-mode t))
    (setq sml-modeline-len 24)
    (setq sml-modeline-numbers 'none)
    )
  (message "  2.38 sml modeline... Done"))

;;
;;; DIRED
(when section-mode-dired (message "  2.39 Dired mode...")
  ;; change size display in Dired mode
  (setq dired-listing-switches "-alh")
  (message "  2.39 Dired mode... Done"))

;;
;;; ISEARCH
(when section-mode-isearch (message "  2.40 isearch mode...")
  (setq isearch-allow-scroll t)
  (message "  2.40 isearch mode... Done"))

;;
;;; RAINBOW DELIMITERS
(when section-mode-rainbow-delimiters (message "  2.41 Rainbow Delimiters...")
  (when (try-require 'autoload-rainbow-delimiters "    ")
    ;; set dark background
    (setq-default frame-background-mode 'dark)
    (when (and section-environment-version-recognition running-on-emacs-24)
      ;; enable this mode in programming mode
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
    )
  (message "  2.41 Rainbow Delimiters... Done"))

;;
;;; CALFW
;; a more graphical calendar
;;;; need to clarify openssl.exe with google.com
(when section-mode-calfw (message "  2.42 Calfw...")
  (when (try-require 'calfw-ical "    ")
    ;; Month
    (setq calendar-month-name-array
      ["January" "February" "March"     "April"   "May"      "June"
        "July"    "August"   "September" "October" "November" "December"])

    ;; Week days
    (setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

    ;; First day of the week
    (setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

    (cfw:open-ical-calendar profile-google-calendar-url)
    ) ; (when (try-require 'calfw-ical "    ")
  (message "  2.42 Calfw... Done"))

;;
;;; DIRED DETAILS
;; show hide details in dired mode
(when section-mode-dired-details (message "  2.43 Dired Details...")
  (try-require 'autoload-dired-details+ "    ")
  (message "  2.43 Dired Details... Done"))

;;
;;; SMART TAB
;; expand or indent at the point with tab
(when section-mode-smart-tab (message "  2.44 Smart Tab...")
  ;; smart-tab mode has been patch to not change habits about tab key
  ;; Tab key once will indent like always
  ;; Tab key twice will try to expand the current 'expression'
  (when (try-require 'autoload-smart-tab "    ")
    ;; use hippie expand see `.emacs.d/dotemacs/completion.el'
    (setq smart-tab-using-hippie-expand t)
    ;; enable smart-tab mode everywhere
    (global-smart-tab-mode t)
    )
  (message "  2.44 Smart Tab... Done"))

;;
;;; FOLD DWIM
;; show hide code source block
(when section-mode-fold-dwim (message "  2.45 Folding DWIM...")
  (try-require 'fold-dwim "    ")
  (message "  2.45 Folding DWIM... Done"))

;;
;;; DIRED LETTER ISEARCH
;; activate by default isearch in dired mode (not found ??)
(when section-mode-dired-lis (message "  2.46 Dired Letter ISearch...")
  (when (try-require 'autoload-dired-lis "    ")
    (global-dired-lis-mode))
  (message "  2.46 Dired Letter ISearch... Done"))

;;
;;; NXHTML
;; enhance html mode
(when section-mode-nxhtml (message "  2.47 nXhtml...")
  (load (concat (file-name-as-directory dotemacs-path) "plugins/nxhtml/autostart.el"))
  (message "  2.47 nXhtml... Done"))

;;
;;; FASTNAV
;; fast navigation
(when section-mode-fastnav (message "  2.48 FastNav...")
  (when (try-require 'autoload-fastnav "    ")
    ;;(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
    ;;(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
    (global-set-key "\M-s" 'fastnav-jump-to-char-forward)
    ;;(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
    ;;(global-set-key "\M-r" 'fastnav-replace-char-forward)
    ;;(global-set-key "\M-R" 'fastnav-replace-char-backward)
    ;;(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
    ;;(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
    ;;(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
    ;;(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
    ;;(global-set-key "\M-k" 'fastnav-delete-char-forward)
    ;;(global-set-key "\M-K" 'fastnav-delete-char-backward)
    ;;(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
    ;;(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
    ;;(global-set-key "\M-p" 'fastnav-sprint-forward)
    ;;(global-set-key "\M-P" 'fastnav-sprint-backward)
    )
  (message "  2.48 FastNav... Done"))

;;
;;; MRU YANK
;; MRU (Most Recently Used) in kill-ring
(when section-mode-mru-yank (message "  2.49 MRU Yank...")
  (try-require 'MRU-yank "    ")
  (setq MRU-yank-mode t)
  (message "  2.49 MRU Yank... Done"))

;;
;;; ACK
;; search with ack (no more grep)
;; need a patched ack (patch from https://github.com/blixtor/ack/commit/e9ee7ff0e32da86011418dcb9d52c25b1b6d8bdb by blixtor)
;; the ack-standalone present in .emacs.d/plugins/ folder is already patched
(when section-mode-ack (message "  2.50 ACK...")
  (when section-mode-ack-full
    (autoload 'ack-same "full-ack" nil t)
    (autoload 'ack "full-ack" nil t)
    (autoload 'ack-find-same-file "full-ack" nil t)
    (autoload 'ack-find-file "full-ack" nil t)

    (setq ack-executable (concat "perl " (file-name-as-directory dotemacs-path) "plugins/ack-standalone"))
    (setq ack-prompt-for-directory t)
    )
  (when section-mode-ack-and-half
    (autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
    (autoload 'ack-and-a-half "ack-and-a-half" nil t)
    (autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
    (autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
    )
  (when section-mode-ack-emacs
    (try-require 'ack-emacs "    ")
    (setq ack-command (concat (file-name-as-directory dotemacs-path) "plugins/ack-standalone"))
    )
  (message "  2.50 ACK... Done"))

;;
;;; ACE JUMP
;; move quickly and easily with ace jump see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
(when section-mode-ace-jump (message "  2.51 ACE Jump...")
  (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
  ;; to enable jump back
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
  (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
  ;; to enable only in the current window
  (eval-after-load "ace-jump-mode" '(setq ace-jump-mode-scope 'window))
  (message "  2.51 ACE Jump... Done"))

;;
;;; DIREDFUL
;; add color to dired
(when section-mode-diredful (message "  2.52 Diredful...")
  ;; set file conf path must be set before load diredful
  (custom-set-variables
    '(diredful-init-file (concat (file-name-as-directory dotemacs-path) "dotemacs/diredful-conf.el")))
  (try-require 'diredful "    ")
  (message "  2.52 Diredful... Done"))

;;
;;; PS2PDF
;; print buffer/region in pdf (the pdf background is unavoidably white so dark
;; theme don't render good)
(when section-mode-ps2pdf (message "  2.53 PS2PDF...")
  (try-require 'ps2pdf "    ")
;  (try-require 'w32-winprint)
  (message "  2.53 PS2PDF... Done"))

;;
;;; AUCTEX
;; LaTeX editor
(when section-mode-auctex (message "  2.54 AUCTEX...")
  (try-require 'mode-auctex "      ")
  (message "  2.54 AUCTEX... Done"))

;;
;;; HELM (fork ANYTHING)
;; choose anything with the same nice interface
(when section-mode-helm (message "  2.55 Helm...")
  (add-to-list 'load-path  (concat (file-name-as-directory dotemacs-path) "plugins/helm-master"))
  (when (try-require 'helm-config "    ")
    (setq helm-candidate-separator
      "--separator------------------------------")
    (when section-mode-cedet-ecb
    ;;  ;; hide compile window when quit helm
    ;;  (add-hook 'helm-cleanup-hook 'ecb-toggle-compile)
      ;; quit helm when hide compile window
      (add-hook 'ecb-toggle-compile-hide-hook 'helm-keyboard-quit))
    (when section-mode-helm-buffers-list
      ;; to avoid error with helm-buffers-list
      (setq ido-use-virtual-buffers nil))
    ;(add-to-list 'helm-completing-read-handlers-alist)
    ;(setq helm-completing-read-handlers-alist
    ;  (append 'helm-completing-read-handlers-alist
    ;    '((execute-extended-command . nil))
    ;    ))
    ;; enable helm for completing-read and read-file-name command
    ;(helm-mode t)
    )
  (message "  2.55 Helm... Done"))

;;
;;; YASCROLL
;; add a small visual scroll-bar (can not be used with mouse click)
;; see https://github.com/m2ym/yascroll-el for screenshot
(when section-mode-yascroll (message "  2.56 Yascroll...")
  (when (try-require 'yascroll "    ")
    (global-yascroll-bar-mode t)
    ;; to always show scroll bar
    (setq yascroll:delay-to-hide profile-yascroll-delay-to-hide))
  (message "  2.56 Yascroll... Done"))

;;
;;; SMART-FORWARD
(when section-mode-smart-forward (message "  2.57 Smart-forward...")
  (add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "plugins/expand-region"))
  (when (try-require 'expand-region "    ")
    (try-require 'smart-forward "    "))
  (message "  2.57 Smart-forward... Done"))

;;
;;; RAINBOW MODE
  ;; show string color in color in lisp
(when section-mode-rainbow (message "  2.58 Rainbow...")
  (try-require 'autoload-rainbow-mode "    ")
  (message "  2.58 Rainbow... Done"))

;;
;;; EDIFF
;; graphical diff (## to toggle whitespace ignoring)
(when section-mode-ediff (message "  2.59 Ediff...")
  (when (try-require 'ediff "    ")
    ;; always split with two vertical buffer in ediff mode
    ;;(add-hook 'ediff-before-setup-hook 'new-frame)
    ;;(add-hook 'ediff-quit-hook 'delete-frame)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-horizontally))
  (message "  2.59 Ediff... Done"))

;;
;;; MAGIT
;; use git with nice interface (do not use vc interface from emacs)
;; under windows you can use msys make (after edit of Makefile) to install magit
(when section-mode-magit (message "  2.60 Magit...")
  (add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "plugins/magit"))
  (when (try-require 'magit "    ")
    (setq magit-git-executable profile-magit-exec)
    (setq magit-commit-all-when-nothing-staged t))
  (message "  2.60 Magit... Done"))

;;
;;;
;;;; SYNERGY
;; use synergy without java client GUI (do not use vc interface from emacs)
(when section-mode-synergy (message "  2.61 Synergy...")
  (when (try-require 'synergy-web "    ")
    (setq synergy-username profile-synergy-username)
    (setq synergy-database profile-synergy-database)
    (setq synergy-server profile-synergy-server)
    (setq synergy-history-filter (append profile-synergy-history-filter synergy-history-filter))
    (setq synergy-diff-external-command profile-synergy-diff-external-command)
    (setq synergy-diff-external-parameter profile-synergy-diff-external-parameter)
    (setq synergy-diff-external-swap-file profile-synergy-diff-external-swap-file))
  (message "  2.61 Synergy... Done"))

;;
;;; DIMINISH
;;              must be load after all other modes
;; shrink major and minor mode name in the modeline
(when section-mode-diminish (message "  2.99 Diminish...")
  (when (try-require 'diminish "    ")
    (eval-after-load "abbrev"
      '(diminish 'abbrev-mode " Ab"))
    (eval-after-load "yasnippet"
      '(diminish 'yas/minor-mode " Y"))
    (eval-after-load "gtags"
      '(diminish 'gtags-mode " G"))
    (eval-after-load "undo-tree"
      '(diminish 'undo-tree-mode " UndoT"))

    (add-hook 'emacs-lisp-mode-hook
      (lambda()
        (setq mode-name "el")))
    )
  (message "  2.99 Diminish... Done"))

(custom-set-variables
;;
;;; HIDE IFDEF
  '(hide-ifdef-initially t)
  '(hide-ifdef-lines nil)
  '(hide-ifdef-shadow t)
  '(hs-hide-comments-when-hiding-all nil)
  )

;;
;;; TRY

;; need to try
;(autoload 'ifdef:ifdef-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:ifndef-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:ifdef-else-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:if-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:if-else-region "ifdef" "ifdef your code" t)


(provide 'mode)

;;; mode.el ends here
