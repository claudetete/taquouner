;;; emacs.el --- config file for Emacs

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

;; Keywords: config, emacs
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 7.0
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `.emacs' (in home directory)
;;

;; GLOSSARY
;; each word between " in definition are defined here
;;
;;   Backup file: by default Emacs create a backup file of each saved file
;;                (ex: `emacs.el' (the file) and emacs.el~ (the backup file))
;;
;;   Buffer: each opened files (ex: `emacs.el'), Emacs window (ex: *scratch*,
;;           *Messages*), some window mode (*Completions*, *ECB Analyze*)...
;;
;;   Completion: tool to complete the word you are typing (M-/)
;;
;;   Customize: Emacs/"mode" settings saved by the interface
;;
;;   Cygwin: unix environment in a MS Windows world
;;
;;   Dired: explorer "mode", to explore file and folder
;;
;;   Elisp: language for "mode" and setting of Emacs (ex: this file)
;;
;;   Grep: tool to match some string/"regex" in text
;;
;;   Kill: 'Cut' some text and put in a clipboard with ring memory (ex: you
;;         can "kill" (M-w) any text each time you want, after that you can
;;         "yank" it
;;
;;   Minibuffer: last line in Emacs interface (kind of status bar) but some
;;               time can be editable to call a function, put parameter, etc
;;
;;   Mode: plugin, extension, add-on, of Emacs to add functionalities (ex:
;;         new language, make coffee, etc)
;;
;;   nil: empty, null, 0, nothing, false, etc (see t)
;;
;;   Regex; regular expressions (see wikipedia)
;;
;;   Server: service/daemon whatever the name of a kind of process run in
;;           background which can intercept each call of Emacs to have only
;;           one instance of Emacs
;;
;;   Snippet: kind of shortcut to put formated block of code (ex: type 'if('
;;            and it put the following text:
;;            if () {
;;              // and put the cursor here
;;            }
;;
;;   Speedbar: another window outside of Emacs which can be used by a "mode"
;;
;;   t : true, active, 1, something, etc (see nil)
;;
;;   Tag: some word in a language which has mean (ex: a variable, a function
;;        , etc) (see etags, gnu/global, semantic)
;;
;;   Window: It is not the whole window (point of view of windows manager)
;;           but each frame in Emacs ('C-x 2' or 'C-x 3' to split, C-x 0 to
;;           remove, etc) where each "buffer" can be display
;;
;;   Yank: 'Paste' some "killed" text in the ring memory (C-y last "kill",
;;         M-y roll in the ring memory)
;;

;; TODO
;;  - try browse-kill-ring mode
;;  - use mark in a buffer with bm.el mode
;;
;; DONE
;;  - put all comments in english
;;  - try rect-mark mode/plugin (use CUA rectangle mode)
;;  - make a option to have cedet in Emacs or from repo
;;  - configure semantic
;;  -- use gcc to hide/show #ifdef... (can be done with EDE and semantic but
;;     doesn not work correctly (GNU Global fault)) but it is configured for
;;     preproc macro
;;  - manage properly windows or linux
;;  - use gcc for preprocessing of ifdef etc
;;  - use GNU/Global to manage project and tag
;;  - try system of project/session (not used)
;;  - use pc-keys.el mode
;;  - put condition to make it parametrable
;;  - put message when loading
;;  - manage environment
;;  - switch buffer without all *buffer*
;;  - show current function in ecb methods window
;;  - put colors with grep (remove underline and put in grey background)
;;  - grep definition and declaration with default path
;;  - configure ecb + shortcut + bug opening new window
;;  - use etags (and not ctags) with shortcut M-.  et C-/
;;  - configure speedbar (it's a pain without good shortcut */
;;  - add shortcut and function for header
;;  - add speedbar
;;  - add bookmark + shortcut
;;  - line number
;;  - selection with shift
;;  - selection in color
;;  - font
;;  - highlight current line
;;  - color code
;;  - completion is case sensitive
;;  - move cursor by word/paragraph with M-arrow
;;  - do not type yes when quit
;;  - insert do a delete
;;  - remove trailling space
;;  - use dired+ mode to comfy browsing
;;  - add "end" and "home" x1, x2 and x3

;;; Change Log:
;; 2012-05-02 (7.0)
;;    fix bug about fullscreen (must be set after a change of font) + add some
;;    mode
;; 2012-04-23 (6.9)
;;    add section mouse avoidance
;; 2012-04-19 (6.8)
;;    add dictionary
;; 2012-04-04 (6.7)
;;    add perl language
;; 2012-04-01 (6.6)
;;    add elpa package manager + translate some comment in english + google
;;    calendar
;; 2012-03-27 (6.5)
;;    add outline mode + auto highlight symbol minor mode
;; 2012-03-20 (6.4)
;;    add rtrt script mode + vc clearcase
;; 2012-03-03 (6.3)
;;    add a glossary + comment for each option
;; 2012-03-02 (6.2)
;;    add some option about cedet and semantic
;; 2012-02-28 (6.1)
;;    change hard path by a variable path which is defined in .emacs
;; 2012-02-27 (6.0)
;;    add option for annoying things
;; 2011-12-08 (5.9)
;;    add shortcut for semantic
;; 2011-11-18 (5.8)
;;    add semantic option + increase size of *Messages*
;; 2011-09-07 (5.7)
;;    put loading of custom file at the end
;; 2011-07-09 (5.6)
;;    add section terminal vs graphics
;; 2011-07-09 (5.5)
;;    rename indentation to languages
;; 2011-06-10 (5.4)
;;    add eproject, must try it without ecb
;; 2011-05-25 (5.3)
;;    add some option about tags
;; 2011-03-10 (5.2)
;;    split dot emacs file
;; 2010-11-19 (5.1)
;;    add new shortcut for align-regexp
;; 2010-11-09 (5.0)
;;    add mode yasnippet
;; 2010-11-03 (4.9)
;;    date in minibuffer and in french format
;; 2010-11-02 (4.8)
;;    messages + condition (parametrable)
;; 2010-10-21 (4.7)
;;    number window navigation, brace func, size window
;; 2010-10-07 (4.6)
;;    some minor modification
;; 2010-10-05 (4.5)
;;    add semantic for ecb methods
;; 2010-09-16 (4.4)
;;    shortcut buffer + selectword + colors ecb
;; 2010-08-31 (4.3)
;;    utf-8 and function for grep + etags
;; 2010-08-26 (4.2)
;;    add shortcut for grep + etags
;; 2010-08-12 (4.1)
;;    add ecb (Emacs Code Browser)
;; 2010-07-09 (4.0)
;;    speedbar + font + shortcut + color + ido
;; 2010-06-11 (3.5)
;;    shift=selection + number line + shortcut
;; 2010-06-13 (3.0)
;;    selection + highlight line + shortcut + function
;; 2006-10-13 (2.0)
;;    indentation + shortcut + mouse
;; 2006-10-13 (1.0)
;;     trailingwhitespace + mode + toolbar + scrollbar
;; 2006-10-13 (0.1)
;;    creation from scratch + column + startup-message (no dated history
;;    since) until 2010...


;;; Code:
;;
;;;  INIT
(message "--[ Loading my Emacs 23.4 init file ]--")

;; start the emacs server to have only one emacs client
;;(if (window-system)
;;  (server-start))

;; debug this fichier if error
(setq debug-on-error t)

;; increase the size of the log *Messages*
(custom-set-variables
  '(message-log-max 1000))


;;
;;; WORKING ENVIRONMENT
;; this will change some path, function and settings.
;; to work it must be define with one of following:
;;                                      "Magneti Marelli"
;;                                      "Alstom Transport"
;;                                      "LEA"
;;                                      "LEA-arch"
;;                                      "Xebeche"
;;                                      "epita"
;;                                      "default"
(defvar clt-working-environment "default")

;;
;;;   DOTEMACS PATH
;; define path of dotemacs
(cond
;; "cond" is a block of code where each "line" (condition sequence)
  ;; Magneti Marelli
  ((string= clt-working-environment "Magneti Marelli")
    (defvar dotemacs-path "d:/cygwin/usr/bin/.emacs.d"))
  ;; Alstom Transport
  ((string= clt-working-environment "Alstom Transport")
    ;; some section are shunted in dotemacs/environment.el for terminal
    (if (window-system)
      (defvar dotemacs-path "d:/Users/ctete/tools/.emacs.d")
      (defvar dotemacs-path "/cygdrive/d/Users/ctete/tools/.emacs.d")))
  ;; LEA
  ((string= clt-working-environment "LEA")
    (defvar dotemacs-path "r:/Configuration/.emacs.d"))
  ;; LEA-arch
  ((string= clt-working-environment "LEA-arch")
    (defvar dotemacs-path "~/.emacs.d"))
  ;; Xebeche
  ((string= clt-working-environment "Xebeche")
    (defvar dotemacs-path "~/.emacs.d"))
  ;; epita
  ((string= clt-working-environment "epita")
    (defvar dotemacs-path "~/.emacs.d"))
  ;; default
  ((string= clt-working-environment "default")
    ;; !!!
    ;; !!! all sections are overiden in dotemacs/environment.el
    ;; !!!
    (defvar dotemacs-path "~/.emacs.d"))
  )


;;
;;; SETTINGS
;; (defvar section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' or 't' value
;;
;;; ENVIRONMENT                                                         0
;; FILE: dotemacs/environment.el
(defvar section-environment t)
(progn
  ;; OS RECOGNITION                                                     0.1
  (defvar section-environment-os-recognition t)
  ;;
  ;; TERMINAL VS GRAPHICS                                               0.2
  (defvar section-environment-terminal-vs-graphics t)
  ;;
  ;; WORKING ENVIRONMENT                                                0.3
  ;; REQUIREMENT:       section-environment-terminal-vs-graphics
  (defvar section-environment-working-message t)
  ;;
  ;; CYGWIN                                                             0.4
  ;; REQUIREMENT:       section-environment-os-recognition
  ;;                    section-environment-terminal-vs-graphics
  (defvar section-environment-cygwin t)
  ;;
  ;; MS WINDOWS PERFORMANCE                                             0.5
  ;; REQUIREMENT:       section-environment-os-recognition
  ;; improve performance
  (defvar section-environment-ms-windows-performance t)
  ;;
  ;; EXECUTABLE                                                         0.6
  ;; REQUIREMENT:       section-environment-os-recognition
  ;;                    section-environment-terminal-vs-graphics
  (defvar section-environment-executable t)
  ;;
  ;; ELPA                                                               0.7
  ;; with Emacs 24 for multiple repo and up to date
  (defvar section-environment-elpa t)
  ) ; progn


;;
;;; EXTERN FILES                                                        1
;; FILE: dotemacs/externfiles.el
(defvar section-external t)
;; load extern files which are not modes
(progn
  ;; DIRECTORY                                                          1.1
  ;; add "plugins/" to load path
  (defvar section-external-directory t)
  ;;
  ;; FONCTIONS                                                          1.2
  ;; FILE: dotemacs/functions.el
  ;; REQUIREMENT:       section-environment-os-recognition
  ;; load custom function
  (defvar section-external-functions t)
  ;;
  ;; VECTRA                                                             1.3
  ;; FILE: plugins/vectra.el
  ;; man and doc in emacs (never used)
  (defvar section-external-vectra nil)
  ;;
  ;; SETNU                                                              1.4
  ;; FILE: plugins/setnu.el
  ;; display line number at each line (deprecated exist in emacs)
  (defvar section-external-setnu nil)
  ;;
  ;; HOME/END                                                           1.5
  ;; FILE: plugins/pc-keys.elc
  ;; add some usefull function to home and end keys
  (defvar section-external-home-end t)
  ) ; (progn


;;
;;; MODE                                                                2
;; FILE: dotemacs/mode.el
(defvar section-mode t)
;; load extern files which are modes in plugins/
(progn
  ;; DOXYMACS                                                           2.1
  ;; emacs interface for doxygen comments
  (defvar section-mode-doxymacs nil)
  ;;
  ;; IDO                                                                2.2
  ;; yet another switch buffer
  (defvar section-mode-ido nil)
  ;;
  ;; UNIQUIFY                                                           2.3
  ;; create unique buffer name
  (defvar section-mode-uniquify t)
  ;;
  ;; CEDET                                                              2.4
  ;; "Collection of Emacs Development Environment Tools"
  (defvar section-mode-cedet t)
  ;; if you want to use emacs included cedet set to nil
  ;; otherwise set the path of cedet.el and you need to remove:
  ;;    "your-emacs-path/lisp/cedet"
  ;;    "your-emacs-path/lisp/speedbar.*"
  ;;    "your-emacs-path/lisp/emacs-lisp/eieio*"
  ;;;; see in dotemacs/environment.el
  ;;(defvar clt-cedet-path (concat dotemacs-path "/plugins/cedet-snap/common/cedet.elc"))
  (progn
    ;; SEMANTIC                                                         2.4.1
    ;; FILE: dotemacs/mode-semantic.el
    ;; can do tag, list of function/variable..., preproc, etc
    (defvar section-mode-cedet-semantic t)
    ;;
    ;; ECB                                                              2.4.2
    ;; FILE: dotemacs/mode-ecb.el
    ;; "Emacs Code Browser"
    ;; can display other windows or speedbar to view folder tree, source
    ;; list, variable/function list, buffer history, etc
    (defvar section-mode-cedet-ecb t)
    ) ; (progn

  ;;
  ;; BATCH                                                              2.5
  ;; mode for .bat script in MS Windows
  (defvar section-mode-batch t)
  ;;
  ;; VISUAL BASIC                                                       2.6
  ;; mode for VisualBasic and VisualBasicAdvance
  (defvar section-mode-vb t)
  ;;
  ;; WINDOW NUMBERING                                                   2.7
  ;; give a number of each window to easly jump in it
  (defvar section-mode-window-numbering nil)
  ;;
  ;; C                                                                  2.8
  ;; define new type in C
  (defvar section-mode-c t)
  (progn
    ;; CWARN                                                            2.8.1
    ;; display small error in source code (forget semi-colon, etc)
    (defvar section-mode-c-cwarn nil)
    ;;
    ;; DATA DEBUG                                                       2.8.3
    ;; ??  (not used)
    (defvar section-mode-c-data-debug nil)
    ) ; (progn

  ;;
  ;; ICOMPLETION                                                        2.9
  ;; more completion in minibuffer
  (defvar section-mode-icompletion nil)

  ;;
  ;; YASNIPPET                                                          2.10
  ;; snippet mode (not used)
  (defvar section-mode-yasnippet nil)

  ;;
  ;; BROWSE KILL RING                                                   2.11
  ;; mode to browse the kill ring memory
  ;; yank only on the first left top window...
  (defvar section-mode-browse-kill-ring t)

  ;;
  ;; MAGNETI MARELLI                                                    2.12
  (defvar section-mode-mm nil)
  (progn
    ;; EOL                                                              2.12.1
    ;; light syntax color for End Of Line file
    (defvar section-mode-mm-eol t)
    ;;
    ;; CAN DBC                                                          2.12.3
    ;; light syntax color for DataBase CAN file
    (defvar section-mode-mm-dbc t)
    ;;
    ;; CCM DIFF                                                         2.12.3
    ;; light syntax color for synergy diff file
    (defvar section-mode-mm-diff t)
    ) ; (progn

  ;;
  ;; DIRED+                                                             2.13
  ;; improve dired mode, color, open with, etc
  (defvar section-mode-dired-plus t)

  ;;
  ;; GNU/GLOBAL                                                         2.14
  ;; Tag management mode
  ;; use modified gtags.el:
  ;; see function to add from function.el and put the absolute path of
  ;; global executable
  (defvar section-mode-gnu-global t)
  ;;
  ;; EPROJECT (grischka)                                                2.15
  ;; project management mode (never used)
  (defvar section-mode-eproject nil)
  ;;
  ;; RTRT SCRIPT                                                        2.16
  ;; rtrt script mode (syntax coloration)
  (defvar section-mode-rtrt-script t)
  ;;
  ;; VC CLEARCASE                                                       2.17
  ;; vc ClearCase mode
  (defvar section-mode-vc-clearcase t)
  ;;
  ;; CLEARCASE                                                          2.18
  ;; ClearCase mode
  (defvar section-mode-clearcase nil)
  ;;
  ;; AUTOHOTKEY                                                         2.19
  ;; AutoHotKey mode
  (defvar section-mode-autohotkey nil)
  ;;
  ;; OUTLINE                                                            2.20
  ;; Outline mode to manually hide/show source code block
  (defvar section-mode-outline t)
  ;;
  ;; AUTO HIGHLIGHT SYMBOL                                              2.21
  ;; to automatically highlight symbol at point
  (defvar section-mode-auto-highlight-symbol t)
  ;;
  ;; GOOGLE CALENDAR                                                    2.22
  ;; to import google calendar
  (defvar section-mode-google-calendar t)
  ;;
  ;; FILL COLUMN INDICATOR                                              2.23
  ;; show a line at fill-column (set at 80 in dotemacs/misc.el
  ;; be careful enable truncate line
  (defvar section-mode-fill-column-indicator nil)
  ;;
  ;; MUSE                                                               2.24
  ;; show a line at fill-column (set at 80 in dotemacs/misc.el
  ;; muse mode to have nice doc
  (defvar section-mode-muse nil)
  ;;
  ;; UNDO TREE                                                          2.25
  ;; replace the undo built in function
  (defvar section-mode-undo-tree t)
  ;;
  ;; CSV                                                                2.26
  ;; parse/edit/sort csv file
  (defvar section-mode-csv t)
  ;;
  ;; SUBVERSION                                                         2.27
  ;; support Subversion 1.7
  (defvar section-mode-subversion t)
  ;;
  ;; DIFF COLOR                                                         2.28
  ;; add color to diff mode
  (defvar section-mode-diff-color t)
  ) ; (progn


;;
;;; LANGUAGES                                                           3
;; FILE: dotemacs/languages.el
(defvar section-languages t)
(progn
  ;; C                                                                  3.1
  ;; set indentation style and preprocessing option
  (defvar section-languages-c t)
  ;;
  ;; LISP                                                               3.2
  ;; set indentation style
  (defvar section-languages-lisp t)
  ;;
  ;; TAB                                                                3.3
  ;; tab always in space
  (defvar section-languages-tabulation t)
  ;;
  ;; RTRT SCRIPT PTU                                                    3.4
  ;; set indentation style
  (defvar section-languages-rtrt-script t)
  ;;
  ;; PERL                                                               3.5
  ;; set indentation style
  (defvar section-languages-perl t)
  ) ; (progn


;;
;;; SELECTION                                                           4
;; FILE: dotemacs/selection.el
;; selection can be kill + selection is highlight + kill->copy in read only
(defvar section-selection t)
(progn
  ;; SHIFT SELECTION                                                    4.1
  ;; selection can be done with shit and arrow keys (default setting since 23.3)
  (defvar section-selection-with-shift nil)
  ) ; (progn


;;
;;; DISPLAY                                                             5
(defvar section-mydisplay t)
(progn
;; WINDOWS/BUFFERS                                                      5.1
;; FILE: dotemacs/display-buffer.el
;; buffers with *buffername* should be displayed in the same window
;; first column in window will display buffer limit
;; next page will leave 5 shared line
  (defvar section-display-windows-buffers t)
  (progn
    ;; TRANSPARENCY                                                     5.1.1
    ;; the whole emacs will be transparent
    (defvar section-display-windows-buffers-transparency t)
    ) ; (progn

  ;;
  ;; SPEEDBAR                                                           5.2
  ;; FILE: dotemacs/display-speedbar.el
  ;; set size and display of speedbar (see GLOSSARY) (no used)
  (defvar section-display-speedbar nil)

  ;;
  ;; FONT                                                               5.3
  ;; FILE: dotemacs/display-font.el
  ;; REQUIREMENT:       section-environment-os-recognition
  ;;                    section-environment-terminal-vs-graphics
  ;; set font in terminal or in graphic
  (defvar section-display-font t)
  (progn
    ;; INTERNATIONAL                                                    5.3.1
    ;; iso or utf-8 or ...  (not used)
    (defvar section-display-font-international t)
    ) ; (progn

  ;;
  ;; COLOR                                                              5.4
  ;; FILE: dotemacs/display-color.el
  ;; set manual color
  (defvar section-display-color t)
  (progn
    ;; PARENTHESES MODE                                                 5.4.1
    ;; matched parentheses are highlight
    (defvar section-display-color-parentheses-mode t)
    ;; PARENTHESES MINIBUFFER                                           5.4.2
    ;; matched parentheses are highlight and if not visible show it in the
    ;; minibuffer
    (defvar section-display-color-parentheses-visible t)
    ;; PARENTHESES HIGHLIGHT                                            5.4.3
    ;; matched parentheses are highlight in rainbow color
    (defvar section-display-color-parentheses-highlight nil)

    ;; COLOR THEME                                                      5.4.4
    ;; set color by color-theme mode
    (defvar section-display-color-theme nil)

    ;; MISC                                                             5.4.4
    ;; REQUIREMENT:     section-environment-terminal-vs-graphics
    ;;                  section-display-color-theme nil
    ;; current line highlight + full syntax coloration
    (defvar section-display-color-misc t)
    ;;
    ;; MODE                                                             5.4.5
    ;; REQUIREMENT:     section-environment-terminal-vs-graphics
    ;; set color for c-mode, cursor and current line
    (defvar section-display-color-mode t)
    ;;
    ;; GREP                                                             5.4.6
    ;; set color for grep window (all search, occur, grep, grep-find, etc)
    (defvar section-display-color-grep t)
    ;;
    ;; ECB                                                              5.4.7
    ;; REQUIREMENT:     section-mode-cedet-ecb
    ;; set color for ecb-mode
    (defvar section-display-color-ecb t)
    ) ; (progn
  ) ; (progn


;;
;;; INTERFACE                                                           6
;; FILE: dotemacs/interface.el
(defvar section-interface t)
;; display buffer name in titlebar (example "<[ foobar.c ]>")
(progn
  ;; DECORATION                                                         6.1
  ;; remove all mouse interface (toolbar, menubar, scrollbar)
  (defvar section-interface-decoration t)
  ;;
  ;; MODELINE                                                           6.2
  ;; FILE: dotemacs/interface-modeline.el
  ;; set some option to add in the grey line at the bottom of each buffer
  (defvar section-interface-modeline t)
  ;;
  ;; FULLSCREEN                                                         6.3
  ;; REQUIREMENT:       section-environment-os-recognition
  (defvar section-interface-fullscreen t)
  ;;
  ;; ECB                                                                6.4
  ;; FILE: dotemacs/interface-ecb.el
  ;; REQUIREMENT:       section-mode-cedet-ecb
  ;; set size, display, refresh and remove opening tips
  (defvar section-interface-ecb t)
  ) ; (progn


;;
;;; COMPLETION                                                          7
;; FILE: dotemacs/completion.el
;; active letter case completion + dynamique completion
(defvar section-completion t)


;;
;;; SHORTCUT                                                            8
(defvar section-shortcut t)
(progn
  ;; GLOBAL                                                             8.1
  ;; FILE: dotemacs/shortcut-global.el
  ;; add global shortcut
  (defvar section-shortcut-global t)
  (progn
    ;; CUA                                                              8.1.1
    (defvar section-shortcut-global-cua nil)
    ) ; (progn

  ;; WINDOWS                                                            8.2
  ;; FILE: dotemacs/shortcut-windows.el
  ;; add shortcut to manage windows
  (defvar section-shortcut-windows t)
  ;;
  ;; BUFFERS                                                            8.3
  ;; FILE: dotemacs/shortcut-buffers.el
  ;; add shortcut to manage buffers
  (defvar section-shortcut-buffers t)

  ;; ECB                                                                8.4
  ;; FILE: dotemacs/shortcut-ecb.el
  ;; REQUIREMENT:       section-mode-cedet-ecb
  ;; add shortcut to manage ecb windows
  (defvar section-shortcut-ecb t)

  ;; GREP                                                               8.5
  ;; FILE: dotemacs/shortcut-grep.el
  ;; add shortcut to manage grep
  (defvar section-shortcut-grep t)

  ;; FUNCTION                                                           8.6
  ;; FILE: dotemacs/shortcut-function.el
  ;; add shortcut to manage ecb windows
  (defvar section-shortcut-function t)

  ;; TAGS                                                               8.7
  ;; FILE: dotemacs/shortcut-tags.el
  ;; add shortcut to manage gtags or etags
  (defvar section-shortcut-tags t)
  (progn
    ;; ETAGS                                                            8.7.1
    (defvar section-shortcut-tags-exhuberant-ctags nil)
    ;;
    ;; GTAGS                                                            8.7.2
    ;; REQUIREMENT:       section-mode-gnu-global
    (defvar section-shortcut-tags-gnu-global t)
    ) ; (progn

  ;; SEMANTIC                                                           8.8
  ;; FILE: dotemacs/shortcut-semantic.el
  ;; REQUIREMENT:       section-mode-cedet-semantic
  ;; add shortcut to move in source code with semantic
  (defvar section-shortcut-semantic t)
  ) ; (progn


;;
;;; MOUSE                                                               9
;; FILE: dotemacs/mouse.el
;; REQUIREMENT: section-environment-terminal-vs-graphics
;; smooth wheel + lazy decoration when scroll
(defvar section-mouse t)
(progn
  ;;
  ;; PASTE CURSOR                                                       9.1
  ;; yank at point not mouse cursor (either when yank with mouse wheel)
  (defvar section-mouse-paste-to-point-not-mouse-cursor t)
  ;;
  ;; AVOIDANCE                                                          9.2
  ;; mouse cursor avoid the keyboard cursor when typing
  (defvar section-mouse-avoidance t)
) ; (progn


;;
;;; ANNOYANCES                                                          10
;; FILE: dotemacs/annoyances.el
;; no welcome message + yes->y + do not query to refresh buffer + remove inser
;; key + remove C-Pup & C-Dwn + wheel click od nothing + no dialog box + no
;; tooltips
(defvar section-annoyances t)
(progn
  ;;
  ;; TRUNCATE LINE                                                      10.1
  ;; whole line not visible (need to scroll right)
  (defvar section-annoyances-truncate-line nil)
  ;;
  ;; SCROLL PRESERVE CURSOR POSITION                                    10.2
  ;; when wheel scroll the cursor do not move
  (defvar section-annoyances-scroll-preserve-cursor-position nil)
  ;;
  ;; NO BACKUP FILE                                                     10.3
  ;; no backup file will be created
  (defvar section-annoyances-no-backup-file nil)
  ;;
  ;; ALL BACKUP FILE IN DIRECTORY                                       10.4
  ;; all backup files will be created in a directory
  (defvar section-annoyances-backup-file-in-directory t)
  ) ; (progn


;;
;;; MISC                                                                11
;; FILE: dotemacs/misc.el
;; remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
(defvar section-misc t)
(progn
  ;;
  ;; CALENDAR                                                           11.1
  ;; set latitude/longitude + location + holidays + custom date in modeline
  ;; lunar phase, sunrise/sunset, time etc
  (defvar section-misc-calendar t)
  ;;
  ;; DICTIONARY                                                         11.2
  ;; set default dictionary, etc
  (defvar section-misc-dictionary t)
  ;;
  ;; BOOKMARK                                                           11.3
  ;; set default dictionary, etc
  (defvar section-misc-bookmark t)
  ) ; (progn


;;
;;; CUSTOMIZE                                                           12
;; FILE: dotemacs/custom.el
;; all customize settings are put in here when you use interface (customize)
;; to change settings
(defvar section-filecustomize t)


;;
;;; ENVIRONNEMENT
(when section-environment (message "0 Environment...")
  (load-file (concat dotemacs-path "/dotemacs/environment.el"))
  (message "0 Environment... Done"))

;;
;;; EXTERN FILES
(when section-external (message "1 External files...")
  (load-file (concat dotemacs-path "/dotemacs/externfiles.el"))
  (message "1 External files... Done"))

;;
;;; MODE
(when section-mode (message "2 Mode...")
  (load-file (concat dotemacs-path "/dotemacs/mode.el"))
  (message "2 Mode... Done"))

;;
;;; LANGUAGES
(when section-languages (message "3 Languages...")
  (load-file (concat dotemacs-path "/dotemacs/languages.el"))
  (message "3 Languages... Done"))

;;
;;; SELECTION
(when section-selection (message "4 Selection...")
  (load-file (concat dotemacs-path "/dotemacs/selection.el"))
  (message "4 Selection... Done"))

;;
;;; DISPLAY
(when section-mydisplay (message "5 Display...")
  ;; WINDOWS/BUFFERS
  (when section-display-windows-buffers (message "  5.1 Windows / Buffers...")
    (load-file (concat dotemacs-path "/dotemacs/display-buffer.el"))
    (message "  5.1 Windows / Buffers... Done"))
  ;;
  ;; SPEEDBAR
  (when section-display-speedbar (message "  5.2 SpeedBar...")
    (load-file (concat dotemacs-path "/dotemacs/display-speedbar.el"))
    (message "  5.2 SpeedBar... Done"))
  ;;
  ;; FONT
  (when section-display-font (message "  5.3 Font...")
    (load-file (concat dotemacs-path "/dotemacs/display-font.el"))
    (message "  5.3 Font... Done"))
  ;;
  ;; COLOR
  (when section-display-color (message "  5.4 Color...")
    (load-file (concat dotemacs-path "/dotemacs/display-color.el"))
    (message "  5.4 Color... Done"))
  (message "5 Display... Done"))


;;
;;; INTERFACE
(when section-interface (message "6 Interface...")
  (load-file (concat dotemacs-path "/dotemacs/interface.el"))
  ;; ECB
  (when section-interface-ecb (message "  6.4 Ecb...")
    (load-file (concat dotemacs-path "/dotemacs/interface-ecb.el"))
    (message "  6.4 Ecb... Done"))
  (message "6 Interface... Done"))

;;
;;; COMPLETION
(when section-completion (message "7 Completion...")
  (load-file (concat dotemacs-path "/dotemacs/completion.el"))
  (message "7 Completion... Done"))

;;
;;; SHORTCUT
(when section-shortcut (message "8 Shortcut...")
  ;; GLOBAL
  (when section-shortcut-global (message "  8.1 Global Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-global.el"))
    (message "  8.1 Global Shortcuts... Done"))
  ;;
  ;; WINDOWS
  (when section-shortcut-windows (message "  8.2 Windows Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-windows.el"))
    (message "  8.2 Windows Shortcuts... Done"))
  ;;
  ;; BUFFERS
  (when section-shortcut-buffers (message "  8.3 Buffers Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-buffers.el"))
    (message "  8.3 Buffers Shortcuts... Done"))
  ;;
  ;; ECB
  (when section-shortcut-ecb (message "  8.4 Ecb Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-ecb.el"))
    (message "  8.4 Ecb Shortcuts... Done"))
  ;;
  ;; GREP
  (when section-shortcut-grep (message "  8.5 Grep Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-grep.el"))
    (message "  8.5 Grep Shortcuts... Done"))
  ;;
  ;; FUNCTION
  (when section-shortcut-function (message "  8.6 Functions Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-function.el"))
    (message "  8.6 Functions Shortcuts... Done"))
  ;;
  ;; TAGS
  (when section-shortcut-tags (message "  8.7 Tags Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-tags.el"))
    (message "  8.7 Tags Shortcuts... Done"))
  ;;
  ;; SEMANTIC
  (when section-shortcut-semantic (message "  8.8 Semantic Shortcuts...")
    (load-file (concat dotemacs-path "/dotemacs/shortcut-semantic.el"))
    (message "  8.8 Semantic Shortcuts... Done"))
  (message "8 Shortcut... Done"))

;;
;;; MOUSE
(when section-mouse (message "9 Mouse...")
  (load-file (concat dotemacs-path "/dotemacs/mouse.el"))
  (message "9 Mouse... Done"))

;;
;;; ANNOYANCES
(when section-annoyances (message "10 Annoyances...")
  (load-file (concat dotemacs-path "/dotemacs/annoyances.el"))
  (message "10 Annoyances... Done"))

;;
;;; MISC
(when section-misc (message "11 Misc...")
  (load-file (concat dotemacs-path "/dotemacs/misc.el"))
  (message "11 Misc... Done"))

;;
;;; CUSTOMIZE
(when section-filecustomize (message "12 File custom...")
  ;; customize modification (made by Emacs interface) are put in custom.el
  (setq custom-file (concat dotemacs-path "/dotemacs/custom.el"))
  (load-file (concat dotemacs-path "/dotemacs/custom.el"))
  (message "12 File custom... Done"))

;;; emacs.el ends here
