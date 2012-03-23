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
;; Version: 6.4
;; Created: October 2006
;; Last-Updated: March 2012

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
;;  - make a option to have cedet in Emacs or from repo
;;  - try rect-mark mode/plugin
;;  - put all comments in english
;;  - essayer le mode browse-kill-ring
;;  - utiliser les mark dans un buffer avec le mode bm.el
;;
;; DONE
;;  - configurer semantic
;;  -- utiliser gcc pour changer couleur ifdef (can be done with EDE and
;;     semantic but doesn not work correctly)
;;  - gerer windows ou linux proprement
;;  - utiliser gcc pour faire le preprocessing des ifdef et autres macro
;;  - utiliser GNU/Global pour gerer un projet et tag
;;  - essayer un systeme de projet/session
;;  - utiliser le mode intelligent pc-keys.el
;;  - condition pour rendre parametrable
;;  - message lors du chargement
;;  - gestion de l'environnement
;;  - changement de buffer sans tous les buffers Emacs
;;  - fonction afficher suivant l'edition (voir ecb methods)
;;  - couleurs grep /* virer underline en fond gris */
;;  - grep ou autre pour definition et declaration avec chemin par defaut
;;  - configurer ecb + raccourci + bug ouverture new window
;;  - utiliser etags (et pas ctags) avec raccourci M-.  et C-/
;;  - configurer speedbar  /* chiant sans raccourci correct */
;;  - ajouter raccourci et fonction pour header
;;  - ajouter speedbar
;;  - ajouter bookmark + raccourci
;;  - numeros des lignes
;;  - selection avec shift
;;  - selection en couleur
;;  - police d'ecriture
;;  - ligne courante en surbrillance
;;  - couleur code
;;  - completion respecte la casse
;;  - deplace le curseur par mot/paragraphe avec M-fleches
;;  - ne plus taper yes a la sortie
;;  - insert fais un suppr
;;  - suppression des espaces inutiles en fin de lignes
;;  - utilisation de dired+  qui permet une navigation plus aiser
;;  - ajoute fonctionnalites a "end" et "home" x1, x2 et x3

;;; Change Log:
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
(message "* --[ Loading my Emacs 23.4 Cedet init file ]--")

;; start the emacs server to have only one emacs client
(server-start)

;; debug ce fichier si erreur
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
      (defvar clt-working-environment "Alstom Transport")

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
    (defvar dotemacs-path "d:/Users/ctete/tools/.emacs-cedet.d"))
  ;; LEA
  ((string= clt-working-environment "LEA-arch")
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
    (defvar dotemacs-path "~/.emacs.d"))
  )


;;
;;; SETTINGS
;; (defvar section-xxx X) ou 'xxx' correspond a la section du .emacs
;; et 'X' prend la valeur 'nil' si non inclus et 't' si inclus
;;
;;; ENVIRONMENT                                                         0
;; FILE: dotemacs/environment.el
(defvar section-environment t)
(progn
  ;; OS RECOGNITION                                                     0.1
  (defvar section-environment-os-recognition t)
  ;;
  ;; WORKING ENVIRONMENT                                                0.2
  (defvar section-environment-working-message t)
  ;;
  ;; CYGWIN                                                             0.3
  ;; REQUIREMENT: section-environment-os-recognition: t
  (defvar section-environment-cygwin t)
  ;;
  ;; TERMINAL VS GRAPHICS                                               0.4
  (defvar section-environment-terminal-vs-graphics t)
  ;;
  ;; MS WINDOWS PERFORMANCE                                             0.5
  ;; improve performance
  (defvar section-environment-ms-windows-performance t)
  ;;
  ;; EXECUTABLE                                                         0.6
  ;; REQUIREMENT: section-environment-os-recognition: t
  (defvar section-environment-executable t)
  ) ; progn


;;
;;; INTERFACE                                                           1
;; FILE: dotemacs/interface.el
(defvar section-interface t)
;; display buffer name in titlebar (example "<[ foobar.c ]>")
(progn
  ;; DECORATION                                                         1.1
  ;; remove all mouse interface (toolbar, menubar, scrollbar)
  (defvar section-interface-decoration t)
  ;;
  ;; FULLSCREEN                                                         1.2
  (defvar section-interface-fullscreen nil)
  ;;
  ;; MODELINE                                                           1.3
  ;; set some option to add in the grey line at the bottom of each buffer
  (defvar section-interface-modeline t)
  ) ; (progn


;;
;;; FICHIERS EXTERNES                                                   2
;; FILE: dotemacs/externfiles.el
(defvar section-external t)
;; load extern files which are not modes
(progn
  ;; DIRECTORY                                                          2.1
  ;; add "plugins/" to load path
  (defvar section-external-directory t)
  ;;
  ;; FONCTIONS                                                          2.2
  ;; FILE: dotemacs/functions.el
  ;; load custom function
  (defvar section-external-functions t)
  ;;
  ;; VECTRA                                                             2.3
  ;; FILE: plugins/vectra.el
  ;; man and doc in emacs (never used)
  (defvar section-external-vectra nil)
  ;;
  ;; SETNU                                                              2.4
  ;; FILE: plugins/setnu.el
  ;; display line number at each line (deprecated exist in emacs)
  (defvar section-external-setnu nil)
  ;;
  ;; HOME/END                                                           2.5
  ;; FILE: plugins/pc-keys.elc
  ;; add some usefull function to home and end keys
  (defvar section-external-home-end t)
  ) ; (progn


;;
;;; MODE                                                                3
;; FILE: dotemacs/mode.el
(defvar section-mode t)
;; load extern files which are modes in plugins/
(progn
  ;; DOXYMACS                                                           3.1
  ;; emacs interface for doxygen comments
  (defvar section-mode-doxymacs nil)
  ;;
  ;; IDO                                                                3.2
  ;; yet another switch buffer
  (defvar section-mode-ido nil)
  ;;
  ;; UNIQUIFY                                                           3.3
  ;; create unique buffer name
  (defvar section-mode-uniquify t)
  ;;
  ;; CEDET                                                              3.4
  ;; "Collection of Emacs Development Environment Tools"
  (defvar section-mode-cedet t)
  ;; if you want to use emacs included cedet set to nil
  ;; otherwise set the path of cedet.el and you need to remove:
  ;;    "your-emacs-path/lisp/cedet"
  ;;    "your-emacs-path/lisp/speedbar.*"
  ;;    "your-emacs-path/lisp/emacs-lisp/eieio*"
  (defvar clt-cedet-path (concat dotemacs-path "/plugins/cedet-snap/common/cedet.elc"))
  (progn
    ;; SEMANTIC                                                         3.4.1
    ;; FILE: dotemacs/mode-semantic.el
    ;; can do tag, list of function/variable..., preproc, etc
    (defvar section-mode-cedet-semantic t)
    ;;
    ;; ECB                                                              3.4.2
    ;; FILE: dotemacs/mode-ecb.el
    ;; "Emacs Code Browser"
    ;; can display other windows or speedbar to view folder tree, source
    ;; list, variable/function list, buffer history, etc
    (defvar section-mode-cedet-ecb t)
    ) ; (progn

  ;;
  ;; BATCH                                                              3.5
  ;; mode for .bat script in MS Windows
  (defvar section-mode-batch t)
  ;;
  ;; VISUAL BASIC                                                       3.6
  ;; mode for VisualBasic and VisualBasicAdvance
  (defvar section-mode-vb t)
  ;;
  ;; WINDOW NUMBERING                                                   3.7
  ;; give a number of each window to easly jump in it
  (defvar section-mode-window-numbering nil)
  ;;
  ;; C                                                                  3.8
  ;; define new type in C
  (defvar section-mode-c t)
  (progn
    ;; CWARN                                                            3.8.1
    ;; display small error in source code (forget semi-colon, etc)
    (defvar section-mode-c-cwarn nil)
    ;;
    ;; DATA DEBUG                                                       3.8.3
    ;; ??  (not used)
    (defvar section-mode-c-data-debug nil)
    ) ; (progn

  ;;
  ;; ICOMPLETION                                                        3.9
  ;; more completion in minibuffer
  (defvar section-mode-icompletion nil)

  ;;
  ;; YASNIPPET                                                          3.10
  ;; snippet mode (not used)
  (defvar section-mode-yasnippet nil)

  ;;
  ;; BROWSE KILL RING                                                   3.11
  ;; mode to browse the kill ring memory (never used)
  (defvar section-mode-browse-kill-ring nil)

  ;;
  ;; MAGNETI MARELLI                                                    3.12
  (defvar section-mode-mm nil)
  (progn
    ;; EOL                                                              3.12.1
    ;; light syntax color for End Of Line file
    (defvar section-mode-mm-eol t)
    ;;
    ;; CAN DBC                                                          3.12.3
    ;; light syntax color for DataBase CAN file
    (defvar section-mode-mm-dbc t)
    ;;
    ;; CCM DIFF                                                         3.12.3
    ;; light syntax color for synergy diff file
    (defvar section-mode-mm-diff t)
    ) ; (progn

  ;;
  ;; DIRED+                                                             3.13
  ;; improve dired mode, color, open with, etc
  (defvar section-mode-dired-plus t)

  ;;
  ;; GNU/GLOBAL                                                         3.14
  ;; Tag management mode
  ;; use modified gtags.el:
  ;; see function to add from function.el and put the absolute path of
  ;; global executable
  (defvar section-mode-gnu-global t)

  ;;
  ;; EPROJECT (grischka)                                                3.15
  ;; project management mode (never used)
  (defvar section-mode-eproject nil)

  ;;
  ;; RTRT SCRIPT                                                        3.16
  ;; rtrt script mode (syntax coloration)
  (defvar section-mode-rtrt-script t)

  ;;
  ;; VC CLEARCASE                                                       3.17
  ;; vc ClearCase mode
  (defvar section-mode-vc-clearcase t)

  ;;
  ;; CLEARCASE                                                          3.18
  ;; ClearCase mode
  (defvar section-mode-clearcase nil)

  ;;
  ;; AUTOHOTKEY                                                         3.19
  ;; AutoHotKey mode
  (defvar section-mode-autohotkey nil)
  ) ; (progn


;;
;;; LANGUAGES                                                           4
;; FILE: dotemacs/languages.el
(defvar section-languages t)
(progn
  ;; C                                                                  4.1
  ;; set indentation style and preprocessing option
  (defvar section-languages-c t)
  ;;
  ;; LISP                                                               4.2
  ;; set indentation style
  (defvar section-languages-lisp t)
  ;;
  ;; TAB                                                                4.3
  ;; tab always in space
  (defvar section-languages-tabulation t)
  ) ; (progn


;;
;;; SELECTION                                                           5
;; FILE: dotemacs/selection.el
;; selection can be kill + selection is highlight + kill->copy in read only
(defvar section-selection t)
(progn
  ;; SHIFT SELECTION                                                    5.1
  ;; selection can be done with shit and arrow keys
  (defvar section-selection-with-shift nil)
  ) ; (progn


;;
;;; DISPLAY                                                             6
(defvar section-mydisplay t)
(progn
;; WINDOWS/BUFFERS                                                      6.1
;; FILE: dotemacs/display-buffer.el
;; buffers with *buffername* should be displayed in the same window
;; first column in window will display buffer limit
;; next page will leave 5 shared line
  (defvar section-display-windows-buffers t)
  (progn
    ;; TRANSPARENCY                                                     6.1.1
    ;; the whole emacs will be transparent
    (defvar section-display-windows-buffers-transparency t)
    ) ; (progn

  ;;
  ;; SPEEDBAR                                                           6.2
  ;; FILE: dotemacs/display-speedbar.el
  ;; set size and display of speedbar (see GLOSSARY) (no used)
  (defvar section-display-speedbar nil)
  ;;
  ;; ECB                                                                6.3
  ;; FILE: dotemacs/display-ecb.el
  ;; REQUIREMENT: section-mode-cedet-ecb: t
  ;; set size, display, refresh and remove opening tips
  (defvar section-display-ecb t)

  ;;
  ;; FONT                                                               6.4
  ;; FILE: dotemacs/display-font.el
  ;; set font in terminal or in graphic
  (defvar section-display-font t)
  (progn
    ;; INTERNATIONAL                                                    6.4.1
    ;; iso or utf-8 or ...  (not used)
    (defvar section-display-font-international t)
    ) ; (progn

  ;;
  ;; COLOR                                                              6.5
  ;; FILE: dotemacs/display-color.el
  ;; set manual color
  (defvar section-display-color t)
  ;; set color by color-theme mode (no used)
  (defvar section-display-color-theme nil)
  (progn
    ;; MISC                                                             6.5.1
    ;; current line highlight + full syntax coloration
    (defvar section-display-color-misc t)

    ;; PARENTHESES MODE                                                 6.5.2
    ;; matched parentheses are highlight
    (defvar section-display-color-parentheses-mode t)
    ;; PARENTHESES MINIBUFFER                                           6.5.3
    ;; matched parentheses are highlight and if not visible show it in the
    ;; minibuffer
    (defvar section-display-color-parentheses-visible t)
    ;; PARENTHESES HIGHLIGHT                                            6.5.4
    ;; matched parentheses are highlight in rainbow color
    (defvar section-display-color-parentheses-highlight nil)
    ;;
    ;; MODE                                                             6.5.5
    ;; set color for c-mode, cursor and current line
    (defvar section-display-color-mode t)
    ;;
    ;; GREP                                                             6.5.6
    ;; set color for grep window (all search, occur, grep, grep-find, etc)
    (defvar section-display-color-grep t)
    ;;
    ;; ECB                                                              6.5.7
    ;; set color for ecb-mode
    (defvar section-display-color-ecb t)
    ) ; (progn
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
    (defvar section-shortcut-tags-gnu-global t)
    ) ; (progn

  ;; SEMANTIC                                                           8.8
  ;; FILE: dotemacs/shortcut-semantic.el
  ;; add shortcut to move in source code with semantic
  (defvar section-shortcut-semantic t)
  ) ; (progn


;;
;;; MOUSE                                                               9
;; FILE: dotemacs/mouse.el
;; smooth wheel + mouse avoid cursor when typing + lazy decoration when scroll
(defvar section-mouse t)
(progn
  ;;
  ;; PASTE CURSOR                                                       9.1
  ;; yank at point not mouse cursor (either when yank with mouse wheel)
  (defvar section-mouse-paste-to-point-not-mouse-cursor t)
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
  ;; lunar phase and time
  (defvar section-misc-calendar t)
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
;;; INTERFACE
(when section-interface (message "1 Interface...")
  (load-file (concat dotemacs-path "/dotemacs/interface.el"))
  (message "1 Interface... Done"))

;;
;;; FICHIERS EXTERNES
(when section-external (message "2 External files...")
  (load-file (concat dotemacs-path "/dotemacs/externfiles.el"))
  (message "2 External files... Done"))

;;
;;; MODE
(when section-mode (message "3 Mode...")
  (load-file (concat dotemacs-path "/dotemacs/mode.el"))
  (message "3 Mode... Done"))

;;
;;; LANGUAGES
(when section-languages (message "4 Languages...")
  (load-file (concat dotemacs-path "/dotemacs/languages.el"))
  (message "4 Languages... Done"))

;;
;;; SELECTION
(when section-selection (message "5 Selection...")
  (load-file (concat dotemacs-path "/dotemacs/selection.el"))
  (message "5 Selection... Done"))

;;
;;; DISPLAY
(when section-mydisplay (message "6 Display...")

  ;; WINDOWS/BUFFERS
  (when section-display-windows-buffers (message "  6.1 Windows / Buffers...")
    (load-file (concat dotemacs-path "/dotemacs/display-buffer.el"))
    (message "  6.1 Windows / Buffers... Done"))
  ;;
  ;; SPEEDBAR
  (when section-display-speedbar (message "  6.2 SpeedBar...")
    (load-file (concat dotemacs-path "/dotemacs/display-speedbar.el"))
    (message "  6.2 SpeedBar... Done"))
  ;;
  ;; ECB
  (when section-display-ecb (message "  6.3 Ecb...")
    (load-file (concat dotemacs-path "/dotemacs/display-ecb.el"))
    (message "  6.3 Ecb... Done"))
  ;;
  ;; FONT
  (when section-display-font (message "  6.4 Font...")
    (load-file (concat dotemacs-path "/dotemacs/display-font.el"))
    (message "  6.4 Font... Done"))
  ;;
  ;; COLOR
  (when section-display-color (message "  6.5 Color...")
    (load-file (concat dotemacs-path "/dotemacs/display-color.el"))
    (message "  6.5 Color... Done"))
  (message "6 Display... Done"))

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
  ;; les configurations modifiees par l'interface sont enregistres dans
  ;; emacs-custom
  (setq custom-file (concat dotemacs-path "/dotemacs/custom.el"))
  (load-file (concat dotemacs-path "/dotemacs/custom.el"))
  (message "12 File custom... Done"))

;;; emacs.el ends here
