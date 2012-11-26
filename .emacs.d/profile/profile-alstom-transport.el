;;; profile-alstom-transport.el --- a config file for profile

;; Copyright (c) 2012 Claude Tete
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

;; Keywords: config, profile, environment, working
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.2
;; Created: June 2012
;; Last-Updated: November 2012

;;; Commentary:
;;
;; load by `dotemacs/profile.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment-profile'

;;; Change Log:
;; 2012-11-26 (0.4)
;;    add new mode and clean up
;; 2012-06-21 (0.3)
;;    add browser
;; 2012-06-13 (0.2)
;;    add some mode
;; 2012-06-04 (0.1)
;;    creation from scratch


;;; Code:
;; load private variable
(try-require 'profile-alstom-transport-private "    ")

;;
;;; CUSTOM
(setq profile "Alstom Transport")

;; path
(setq profile-path
  (concat
    "d:/User/ctete/tools/MinGW/msys/1.0/bin"                                ";"
    "d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/site/bin"           ";"
    "d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/bin"                ";"
    "d:/Users/ctete/tools/strawberry-perl-5.14.2.1/c/bin"                   ";"
    "d:/cygwin/bin"                                                         ";"
    "d:/cygwin/usr/bin"                                                     ";"
    "d:/cygwin/usr/local/bin"                                               ";"
    "C:/Program Files/IBM/RationalSDLC/ClearCase/bin"                       ";"
    "d:/Users/ctete/tools/gnuwin32/bin"                                     ";"
    "D:/Users/ctete/tools/MikTex/miktex/bin"                                ";"
    "d:/cygwin/bin"                                                         ";"
    "c:/WINDOWS"                                                            ";"
    "c:/WINDOWS/System32"                                                   ";"
    "d:/cygwin/bin"                                                         ";"
    "c:/WINDOWS"                                                            ";"
    "c:/WINDOWS/System32"                                                   ";"
    "/usr/local/bin"                                                        ";"
    "/usr/bin;/bin"                                                         ";"
    "c:/Program Files/IBM/RationalSDLC/common"                              ";"
    "c:/Program Files/PRQA/PDFReports/texmf/miktex/bin"                     ";"
    "c:/Program Files/Analog Devices/VisualDSP"                             ";"
    "c:/Program Files/Analog Devices/VisualDSP/System"                      ";"
    "c:/WINDOWS/system32"                                                   ";"
    "c:/WINDOWS"                                                            ";"
    "c:/WINDOWS/System32/Wbem"                                              ";"
    "c:/Program Files/QuickTime/QTSystem"                                   ";"
    "c:/Program Files/Fichiers communs/Aladdin Shared/eToken/PKIClient/x32" ";"
    "d:/system/Notes"                                                       ";"
    "c:/Program Files/Symantec/pcAnywhere"                                  ";"
    "c:/Program Files/UltraEdit"                                            ";"
    "c:/Program Files/IBM/RationalSDLC/ClearCase/etc/utils"                 ";"
    "c:/Program Files/Rational/TestRealTime/bin/intel/win32"                ";"
    "c:/Program Files/Rational/common"                                      ";"
    "c:/Program Files/Lotus/Notes"                                          ";"
    "c:/Program Files/IBM/RationalSDLC/ClearCase/bin"                       ";"
    "d:/Users/ctete/tools/gnuwin32/bin"                                     ";"
    "C:/Program Files/Aspell/bin/"                                          ";"
    "C:/Python27"                                                           ";"
    "d:/Users/ctete/tools/.emacs.d/plugins/gnu_global_622wb/bin"            ";"
    )
  )

;; executables path
(setq profile-exec-path
  '(
     "d:/User/ctete/tools/MinGW/msys/1.0/bin"
     "d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/site/bin"
     "d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/bin"
     "d:/Users/ctete/tools/strawberry-perl-5.14.2.1/c/bin"
     "d:/cygwin/bin"
     "d:/cygwin/usr/bin"
     "d:/cygwin/usr/local/bin"
     "d:/Users/ctete/tools/gnuwin32/bin"
     "D:/Users/ctete/tools/MikTex/miktex/bin"
     "c:/WINDOWS"
     "c:/WINDOWS/System32"
     "c:/Program Files/IBM/RationalSDLC/ClearCase/bin"
     "C:/Python27"
     "C:/Program Files/Aspell/bin/"
     "c:/Program Files/IBM/RationalSDLC/ClearCase/etc/utils"
     "c:/Program Files/Rational/TestRealTime/bin/intel/win32"
     "c:/Program Files/Rational/common"
     "c:/Program Files/Analog Devices/VisualDSP"
     "c:/Program Files/Analog Devices/VisualDSP/System"
     "d:/Users/ctete/tools/.emacs.d/plugins/gnu_global_622wb/bin"
     "dede"
     )
  )

;; languages
(setq profile-lang "en_US") ; for subversion and Dired

;; shell
(setq profile-shell-file-name "D:/cygwin/bin/bash.exe")
(setq profile-shell-cygwin "d:/cygwin/bin/zsh.exe")

;; ediff
(setq profile-ediff-diff-program "C:/Program Files/GnuWin32/bin/diff.exe")
(setq profile-ediff-diff3-program "C:/Program Files/GnuWin32/bin/diff3.exe")
(setq profile-ediff-cmp-program "C:/Program Files/GnuWin32/bin/cmp.exe")

;; backup directory
(setq profile-backup-directory (concat dotemacs-path "/backup"))
(setq profile-autosave-directory (concat dotemacs-path "/cache"))

;; color theme
(when section-display-color-theme
  (setq profile-color-theme "sweet"))

;; font
;; choice between:
;;; Terminal
;; nice, very tiny, only ascii (too tiny ?)
;; (setq profile-font "Terminal-6")
;;
;;; Anonymous Pro, 10
;; nice, big (slashed 'zero', 'one' and minus 'L' can be mixed up)
;; (setq profile-font "Anonymous Pro-10")
;;; Anonymous Pro, 8
;; nice, small (slashed 'zero', 'one' and minus 'L' can be mixed up, parentheses and curly bracket can be mixed up)
;; (setq profile-font "Anonymous Pro-8")
;;
;;; Proggy Tiny Z, 6
;; good, very tiny (slashed 'zero', dot and comma can be mixed)
(setq profile-font "ProggyTinySZ-6")
;;
;;; DejaVu Sans Mono, 10
;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
;; (setq profile-font "DejaVu Sans Mono-10")
;;; DejaVu Sans Mono, 8
;; (setq profile-font "DejaVu Sans Mono-8")
;;
;;; Inconsolata, 10
;; not so good with ms window (slashed 'zero', capitalized 'i' and minus 'L' can be mixed up)
;; (setq profile-font "Inconsolata-10")
;;
;;; Lucida Console, 10
;; nice, big, large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
;; (setq profile-font "Lucida Console-10")
;;; Lucida Console, 8
;; nice, small large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
;; (setq profile-font "Lucida Console-8")
;;
;;; Monaco, 10
;; nice, very big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
;; (setq profile-font "Monaco-10")
;;; Monaco, 8
;; nice, big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
;; (setq profile-font "Monaco-8")
;;
;;; ProFont, 8
;; nice, tiny, (slashed 'zero', 'one' and minus 'L' can be mixed up)
;; (setq profile-font "ProFontWindows-8")
;;
;;; Courier New 10
;; classic but big and large
;; (setq profile-font "Courier New-10")
;;; Courier New 8
;; classic but big and large
;; (setq profile-font "Courier New-8")

;; indentation offset in C
(setq profile-c-indent-offset 3)
;; new type
(setq profile-c-extra-types
  '(
     "ubyte"
     "ushort"
     "ulong"
     "ulonglong"
     "sbyte"
     "sshort"
     "slong"
     "slonglong"
     )
  )


;; could be set in ede project
(setq profile-c-macro-preprocessor "cpp -C")
(setq profile-c-macro-cppflags "-D__DEBUG__")

;; lisp
(setq profile-lisp-indent-offset 2)

;; perl
(setq profile-perl-indent-offset 2)

;; use by fill-xxx or fill column indicator mode
(setq profile-fill-column 80)

;; dictionary
(setq profile-ispell-program "aspell")
(setq profile-ispell-dictionary "english")

;; cedet
(setq profile-gnu-global (concat dotemacs-path "/plugins/gnu_global_622wb/bin/global.exe"))
(setq profile-gnu-global-gtags (concat dotemacs-path "/plugins/gnu_global_622wb/bin/gtags.exe"))
;; if you want to use emacs included CEDET set to nil
;; otherwise set the path of cedet.el and you need to remove:
;;    "your-emacs-path/lisp/cedet"
;;    "your-emacs-path/lisp/speedbar.*"
;;    "your-emacs-path/lisp/emacs-lisp/eieio*"
(setq profile-cedet-path (concat dotemacs-path "/plugins/cedet-1.1/common/cedet.elc"))
;(setq profile-cedet-path (concat dotemacs-path "/plugins/cedet-bzr/trunk/cedet-devel-load.el"))

;; ecb
;; set default path in "ecb directories"
(setq profile-ecb-source-path
  '(
     ;; before is put the EDE projects (see project.el)
     ("m:/e_ctete/a2kc/test/CCN4/test_s/pm4s/RTRT/"          "PM4S_RTRT")
     ("m:/e_ctete/a2kc/test/CCN4/test_s/puma/TestU/sharc/"   "PUMA_RTRT")
     ("d:/Documents and Settings/100516805/Application Data" "/home")
     ("m:/"                                                  "/ClearCase")
     ("d:/Users/ctete"                                       "/Users")
     ("d:/Users/ctete/tmp"                                   "/tmp")
     )
  )
;; regexp of folder to exclude in "ecb directories"
(setq profile-ecb-excluded-directories-regexps
  '(
     "^\\.+$"
     "^\\(TOTO\\|TITI\\)$"
     "\\(Cvisualdspplus2\\|RTRT_res\\)$" ; RTRT
     "\\(TOTO\\|TITI\\)$"                ; example
     )
  )
;; files to be ignored in "ecb source" !! RTFM !!
(setq profile-ecb-source-file-regexps
     '((".*"
         ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|ri2\\|fdc\\|map\\|lis\\|a\\|so\\|tcl\\|err\\|i\\|met\\|merge\\|contrib\\|summary\\.txt\\|atc\\.txt\\)$\\)\\)")
         ("^\\.\\(emacs\\|gnus\\)$"))
        )
  )
;; files to be ignored from Version Control VC
(setq profile-ecb-sources-exclude-cvsignore
  '(
     "_ccmwaid.inf"
     )
  )
;; regexp to form group in "ecb history"
(setq profile-ecb-history-make-buckets
  '(
     "ccn4_pm4s"
     "include"
     "\\.muse$"
     "\\.ptu$"
     "\\.ahk$"
     "\\.[hc][p]*$"
     "\\.el$"
     )
  )

;; clearcase
(setq profile-clearcase-vtree "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/clearvtree.exe")
(setq profile-cleartool "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/cleartool.exe")

;; transparency of the window. 0=transparent/100=opaque
(setq profile-transparency 96)

;; height of modeline for powerline mode
(setq profile-powerline-size "small")

;; browser to open url
(setq profile-browser "D:/Users/ctete/tools/OperaPortable/OperaPortable.exe")

(setq profile-rss-list
  (list
    '("Clubic"      "http://www.clubic.com/articles.rss")
    '("DansTonChat" "http://feeds.feedburner.com/bashfr")
    )
  )

;; project
;; the order is important: display in reverse order (first->last)
(setq profile-ede-project
  '(
     ;; SeanceManager
     "d:/Users/ctete/projects/SeanceManager/SeanceManager.ede.el"
     ;; PUMA
     "z:/a2kc/soft/ccn4/ccn4_puma/PUMA.ede.el"
     ;; PM4S
     "z:/a2kc/soft/ccn4/ccn4_pm4s/PM4S.ede.el"
     )
  )


;;
;;; SECTION
;; (setq section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' or 't' value (false or true)

;;; ENVIRONMENT
(setq section-environment t)
(progn
  ;; PROFILE: load profile from .emacs.d/profile/ directory
  (setq section-environment-profile t)
  ;; VERSION RECOGNITION: detect system: Emacs version
  (setq section-environment-version-recognition t)
  ;; OS RECOGNITION: detect system: MS Windows or Linux
  (setq section-environment-os-recognition t)
  ;; TERMINAL VS GRAPHICS: detect system: terminal or graphical
  (setq section-environment-terminal-vs-graphics t)
  ;; SET PATH: path for all executables
  ;; REQUIREMENT: `section-environment-os-recognition'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-environment-set-path t)
  ;; MS WINDOWS PERFORMANCE: increase performance on MS Windows
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-ms-windows-performance t)
  ;; EXECUTABLE: set some executable like shell or diff
  ;; REQUIREMENT: `section-environment-os-recognition'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-environment-executable t)
  ;; ELPA: packages system support with repositories
  (setq section-environment-elpa t)
  ;; HYPER: menu key become hyper key (modifier key)
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-hyper t)
  ;; SUPER: windows key become super key (modifier key)
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-super nil)
  ;; SERVER : start a server for emacs client (and have only one instance)
  (setq section-environment-server nil)
  ) ; progn


;;; FUNCTIONS: new functions
(setq section-functions t)
(progn
  ;; MAGNETI MARELLI: load custom function for MM profile
  (setq section-function-mm nil)
  ) ; (progn


;;; MODE: load extern files which are modes in plugins/
(setq section-mode t)
(progn
  ;; DIRECTORY: add "plugins/" to load path
  (setq section-mode-directory t)
  ;; VECTRA: man and doc in emacs (never used)
  (setq section-mode-vectra nil)
  ;; HOME/END: add some useful function to home and end keys
  (setq section-mode-home-end t)
  ;; DOXYMACS: emacs interface for doxygen comments
  (setq section-mode-doxymacs nil)
  ;; IDO: yet another switch buffer
  (setq section-mode-ido nil)
  ;; UNIQUIFY: create unique buffer name
  (setq section-mode-uniquify t)
  ;; CEDET: "Collection of Emacs Development Environment Tools"
  (setq section-mode-cedet t)
  (progn
    ;; SEMANTIC: can do tag, list of function/variable..., preproc, etc
    (setq section-mode-cedet-semantic t)
    ;; ECB: "Emacs Code Browser"
    ;; can display other windows or speedbar to view folder tree, source list,
    ;; variable/function list, buffer history, etc
    (setq section-mode-cedet-ecb t)
    ) ; (progn
  ;; BATCH: mode for .bat script in MS Windows
  (setq section-mode-batch t)
  ;; VISUAL BASIC: mode for VisualBasic and VisualBasicAdvance
  (setq section-mode-vb t)
  ;; WINDOW NUMBERING: give a number of each window to easily jump in it
  (setq section-mode-window-numbering nil)
  ;; C: define new type in C
  (setq section-mode-c t)
  (progn
    ;; CWARN: display small error in source code (forget semi-colon, etc)
    (setq section-mode-c-cwarn nil)
    ;; DATA DEBUG: ??  (not used)
    (setq section-mode-c-data-debug nil)
    ) ; (progn
  ;; ICOMPLETION: more completion in Minibuffer
  (setq section-mode-icompletion nil)
  ;; YASNIPPET: snippet mode
  (setq section-mode-yasnippet nil)
  ;; BROWSE KILL RING: mode to browse the kill ring memory yank only on the
  ;; first left top window...
  (setq section-mode-browse-kill-ring t)
  ;; MAGNETI MARELLI:
  (setq section-mode-mm nil)
  (progn
    ;; EOL: syntax color for End Of Line file
    (setq section-mode-mm-eol t)
    ;; CAN DBC: light syntax color for Database CAN file
    (setq section-mode-mm-dbc t)
    ;; CCM DIFF: light syntax color for synergy diff file
    (setq section-mode-mm-diff t)
    ) ; (progn
  ;; DIRED+: improve Dired mode, color, open with, etc
  (setq section-mode-dired-plus t)
  ;; GNU/GLOBAL: Tag management mode (use modified gtags.el)
  (setq section-mode-gnu-global t)
  ;; EPROJECT (grischka): project management mode (never used)
  (setq section-mode-eproject nil)
  ;; RTRT SCRIPT: rtrt script mode (syntax coloration)
  (setq section-mode-rtrt-script t)
  ;; VC CLEARCASE: vc ClearCase mode
  ;; REQUIREMENT: `profile-clearcase-vtree'
  ;;              `profile-cleartool'
  (setq section-mode-vc-clearcase nil)
  ;; CLEARCASE: ClearCase mode (not used)
  (setq section-mode-clearcase t)
  ;; AUTOHOTKEY: AutoHotKey mode
  (setq section-mode-autohotkey t)
  ;; OUTLINE: Outline mode to manually hide/show source code block
  (setq section-mode-outline t)
  ;; AUTO HIGHLIGHT SYMBOL: to automatically highlight symbol at point
  (setq section-mode-auto-highlight-symbol t)
  ;; GOOGLE CALENDAR: to import Google calendar
  ;; REQUIREMENT: `profile-google-calendar-user'
  ;;              `profile-google-calendar-src'
  ;;              `profile-google-calendar-directory'
  ;;              `profile-google-calendar-url'
  (setq section-mode-google-calendar nil)
  ;; FILL COLUMN INDICATOR: show a line at fill-column (set at 80 in
  ;; dotemacs/misc.el be careful enable truncate line
  (setq section-mode-fill-column-indicator nil)
  ;; MUSE: muse mode to have nice doc
  (setq section-mode-muse nil)
  ;; UNDO TREE: replace the undo built in function
  (setq section-mode-undo-tree t)
  ;; CSV: parse/edit/sort CSV file
  (setq section-mode-csv t)
  ;; SUBVERSION: support Subversion 1.7
  (setq section-mode-subversion nil)
  ;; DIFF COLOR: add color to diff mode
  (setq section-mode-diff-color t)
  ;; DIRED SORT: more option to sort in Dired mode
  (setq section-mode-dired-sort t)
  ;; ORG MODE: to organize everything (also use on Android)
  (setq section-mode-org-mode nil)
  ;; ISEARCH+: add some features to isearch
  (setq section-mode-isearch+ nil)
  ;; PSVN: add geatures to subversion integration
  (setq section-mode-psvn nil)
  ;; POWERLINE: fancy modeline
  (setq section-mode-powerline t)
  ;; NYAN: add bar in modeline given position in buffer
  (setq section-mode-nyan nil)
  ;; SML: add bar in modeline given position in buffer
  (setq section-mode-sml nil)
  ;; DIRED: change option to command ls for dired mode
  (setq section-mode-dired t)
  ;; ISEARCH: scroll is possible when incremental search
  (setq section-mode-isearch t)
  ;; RAINBOW DELIMITERS: scroll is possible when incremental search
  (setq section-mode-rainbow-delimiters nil)
  ;; FOLD DWIM: show hide code source block
  (setq section-mode-fold-dwim t)
  ;; DIRED LETTER ISEARCH: activate by default isearch in dired mode
  (setq section-mode-dired-lis t)
  ;; NXHTML: enhance html mode
  (setq section-mode-nxhtml nil)
  ;; FASTNAV: fast navigation
  (setq section-mode-fastnav t)
  ;; MRU YANK: (Most Recently Used) in kill-ring
  (setq section-mode-mru-yank t)
  ;; ACK: search with ack (no more grep) (need perl interpreter)
  (setq section-mode-ack t)
  (progn
    (setq section-mode-ack-full nil)
    (setq section-mode-ack-and-half nil)
    (setq section-mode-ack-emacs t)
    ) ; (progn
  ;; DIMINISH: shrink major and minor mode name in the modeline
  (setq section-mode-diminish t)
  ) ; (progn


;;; LANGUAGES:
(setq section-languages t)
(progn
  ;; C: set indentation style and preprocessing option
  ;; REQUIREMENT: profile-c-indent-offset
  (setq section-languages-c t)
  (progn
    ;; INDENT PREPROCESSOR: make a #define be align with C code
    (setq section-languages-c-indent-preprocessor nil)
    (setq section-languages-c-hide-show t)
    (progn
      (setq section-languages-c-hide-show-hide-all-at-start nil)
      ) ; (progn
    ) ; (progn
  ;; LISP: set indentation style
  (setq section-languages-lisp t)
  ;; TAB: tab always in space
  (setq section-languages-tabulation t)
  ;; RTRT SCRIPT PTU: set indentation style
  (setq section-languages-rtrt-script t)
  ;; PERL: set indentation style
  (setq section-languages-perl t)
  ) ; (progn


;;; SELECTION: selection can be kill + selection is highlight + kill->copy in
;;; read only
(setq section-selection t)
(progn
  ;; SHIFT SELECTION: selection can be done with shit and arrow keys (default
  ;; setting since 23.3)
  (setq section-selection-with-shift nil)
  ) ; (progn


;;; DISPLAY:
(setq section-display t)
(progn
  ;; WINDOWS/BUFFERS: buffers with *buffername* should be displayed in the same
  ;; window first column in window will display buffer limit, next page will leave
  ;; 5 shared line
  (setq section-display-windows-buffers t)
  (progn
    ;;; VISUAL LINE: word wrap, truncate line without cut word
    ;; END and HOME will go to the end/start of screen line not logical line
    (setq section-display-windows-buffers-visual-line nil)
    ) ; (progn
  ;; SPEEDBAR: set size and display of speedbar (see GLOSSARY) (no used)
  (setq section-display-speedbar nil)
  ;; FONT: set font
  ;; REQUIREMENT: `profile-font'
  (setq section-display-font t)
  (progn
    ;; INTERNATIONAL: ISO or utf-8 or ...  (not used)
    (setq section-display-font-international t)
    ) ; (progn
  ;; COLOR: set color
  (setq section-display-color t)
  (progn
    ;; PARENTHESES MODE: matched parentheses are highlight
    (setq section-display-color-parentheses-mode t)
    ;; PARENTHESES MINIBUFFER: matched parentheses are highlight and if not
    ;; visible show it in the Minibuffer
    (setq section-display-color-parentheses-visible t)
    ;; PARENTHESES HIGHLIGHT: matched parentheses are highlight in rainbow color
    (setq section-display-color-parentheses-highlight nil)
    ;; COLOR THEME: set color by color-theme mode (or manual settings nil)
    (setq section-display-color-theme t)
    (progn ;; only if section-display-color-theme equal 'nil' (do not use it
           ;; with terminal)
      ;; MISC: current line highlight + full syntax coloration
      (setq section-display-color-misc t)
      ;; MODE: set color for c-mode, cursor and current line
      (setq section-display-color-mode t)
      ;; GREP: set color for grep window (all search, occur, grep, grep-find,
      ;; etc)
      (setq section-display-color-grep t)
      ;; ECB: set color for ecb-mode
      ;; REQUIREMENT: `section-mode-cedet-ecb'
      (setq section-display-color-ecb t)
      ) ; (progn
    ) ; (progn
  ) ; (progn


;;; INTERFACE: display buffer name in titlebar (example "<[ foobar.c ]>")
(setq section-interface t)
(progn
  ;; DECORATION: remove all mouse interface (toolbar, menubar, scrollbar)
  (setq section-interface-decoration t)
  ;; MODELINE: set some option to add in the grey line at the bottom of each
  ;; buffer
  (setq section-interface-modeline t)
  ;; TRANSPARENCY: the whole emacs will be transparent
  ;; REQUIREMENT: `profile-transparency'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-interface-transparency t)
  ;; FULLSCREEN:
  ;; REQUIREMENT: `section-environment-os-recognition'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-interface-fullscreen t)
  ;; ECB: set size, display, refresh and remove opening tips
  ;; REQUIREMENT: `section-mode-cedet-ecb'
  (setq section-interface-ecb t)
  (progn
    ;; ECB ASCII TREE: display ascii guides instead of image for arborescence
    ;; tree
    (setq section-interface-ecb-ascii-tree t)
    )
  ) ; (progn


;;; COMPLETION: enable letter case completion + dynamic completion
(setq section-completion t)


;;; SHORTCUT:
(setq section-shortcut t)
(progn
  ;; ALL (GLOBAL): add global shortcut (for whole Emacs)
  (setq section-shortcut-global t)
  (progn
    ;; CUA: enable C-x, C-c, C-v to cut copy paste
    ;; don't recommend it otherwise see http://www.emacswiki.org/CuaMode
    (setq section-shortcut-global-cua nil)
    ) ; (progn
  ;; WINDOWS: add shortcut to manage windows
  (setq section-shortcut-windows t)
  ;; BUFFERS: add shortcut to manage buffers
  (setq section-shortcut-buffers t)
  ;; ECB: add shortcut to manage ecb windows
  ;; REQUIREMENT: `section-mode-cedet-ecb'
  (setq section-shortcut-ecb t)
  ;; GREP: add shortcut to manage grep
  (setq section-shortcut-grep t)
  ;; FUNCTION: add shortcut to manage new functions
  (setq section-shortcut-function t)
  ;; TAGS: add shortcut to manage gtags or etags
  (setq section-shortcut-tags t)
  (progn
    ;; ETAGS:
    (setq section-shortcut-tags-exuberant-ctags nil)
    ;; GTAGS:
    ;; REQUIREMENT: section-mode-gnu-global
    (setq section-shortcut-tags-gnu-global t)
    ) ; (progn
  ;; SEMANTIC: add shortcut to move in source code with semantic
  ;; REQUIREMENT: `section-mode-cedet-semantic'
  (setq section-shortcut-semantic t)
  ) ; (progn


;;; MOUSE: smooth wheel + lazy decoration when scroll
(setq section-mouse t)
(progn
  ;; PASTE CURSOR: yank at point not mouse cursor (either when yank with mouse
  ;; wheel)
  (setq section-mouse-paste-to-point-not-mouse-cursor t)
  ;; AVOIDANCE: mouse cursor avoid the keyboard cursor when typing
  ;; REQUIREMENT: `section-environment-terminal-vs-graphics'
  (setq section-mouse-avoidance t)
) ; (progn


;;; ANNOYANCES: no welcome message + yes->y + do not query to refresh buffer +
;; remove insert key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog
;; box + no tooltips
(setq section-annoyances t)
(progn
  ;; TRUNCATE LINE: whole line not visible (need to scroll right)
  (setq section-annoyances-truncate-line nil)
  ;; SCROLL PRESERVE CURSOR POSITION: when wheel scroll the cursor do not move
  (setq section-annoyances-scroll-preserve-cursor-position nil)
  ;; NO BACKUP FILE: no backup file will be created
  (setq section-annoyances-no-backup-file nil)
  ;; ALL BACKUP FILE IN DIRECTORY all backup files will be created in a
  ;; directory
  (setq section-annoyances-backup-file-in-directory t)
  ;; CLASSIC SCROLL: when scroll at start or end screen with arrow, it will
  ;; always scroll line by line
  (setq section-annoyances-classic-scroll nil)
  ) ; (progn


;;; MISC: remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
;; REQUIREMENT: `profile-username'
;;              `profile-column'
(setq section-misc t)
(progn
  ;; CALENDAR set latitude/longitude + location + holidays + custom date in
  ;; Modeline lunar phase, sunrise/sunset, time etc
  ;; REQUIREMENT: profile-longitude
  ;;              profile-latitude
  ;;              profile-location-name
  (setq section-misc-calendar t)
  (progn
    ;; CALENDAR in French
    (setq section-misc-calendar-french t)
    ) ; (progn
  ;; DICTIONARY: set default dictionary, etc
  ;; REQUIREMENT: `profile-ispell-program'
  ;;              `profile-ispell-dictionary'
  (setq section-misc-dictionary t)
  ;; BOOKMARK: set default bookmark storage
  (setq section-misc-bookmark t)
  ;; SCREENSAVER: Set screensaver when idle time higher than 5 minutes
  (setq section-misc-screensaver nil)
  ) ; (progn


;;; CUSTOMIZE: all customize settings are put in here when you use interface
;; (customize) to change settings
(setq section-filecustomize t)


(provide 'profile-alstom-transport)

;;; profile-alstom-transport.el ends here
