;;; profile-sagem-defense-light.el --- a config file for profile

;; Copyright (c) 2013 Claude Tete
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
;; Version: 0.1
;; Created: June 2013
;; Last-Updated: September 2013

;;; Commentary:
;;
;; load by `dotemacs/profile.el'
;; REQUIREMENT: var     `section-environment-profile'

;;; Change Log:
;; 2013-06-05 (0.1)
;;    creation from alstom transport profile


;;; Code:
;; load private variable
(try-require 'profile-sagem-defense-light-private "      ")

;; Enable/disable section:
;; (setq section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' (disable) or 't' (enable) value
;;
;; Settings:
;; (setq profile-xxx X)

;;; ENVIRONMENT
(setq section-environment t)
(when section-environment
  ;; PROFILE: load profile from .emacs.d/profile/ directory
  (setq section-environment-profile t)
  (when section-environment-profile
    (setq profile "Sagem Defense")
    ) ; (when section-environment-profile
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
  (when section-environment-set-path
    ;; path
    (setq profile-path
      (concat
        "z:/cmbuilder/3.0beta2_KC390"                                ";"
        "c:/cygwin/lib/python2.5"                                    ";"
        "d:/CTE/tools/.emacs.d/plugins/gnu_global_628wb/bin"         ";"
        "c:/perl/perl/site/bin"                                      ";"
        "c:/perl/perl/bin"                                           ";"
        "c:/perl/c/bin"                                              ";"
        "c:/cygwin/bin"                                              ";"
        "c:/cygwin/usr/bin"                                          ";"
        "c:/cygwin/usr/local/bin"                                    ";"
        "c:/MinGW/msys/1.0/bin"                                      ";"
        "c:/Program Files (x86)/gs/gs9.06/bin"                       ";"
        "d:/CTE/tools/MikTex/miktex/bin"                             ";"
        "c:/WINDOWS"                                                 ";"
        "c:/WINDOWS/System32"                                        ";"
        "d:/CTE/tools/GnuWin32/bin"                                  ";"
        "c:/Program Files (x86)/Aspell/bin/"                         ";"
        "d:/Tools/diab/5.8.0.0/WIN32/bin"                            ";"
        "c:/Program Files (x86)/IBM/Rational/Synergy/7.1/bin"        ";"
        "c:/Program Files (x86)/IBM/Rational/Logiscope/6.6/bin"      ";"
        "c:/Program Files (x86)/SYNCHRONe/CCC/3.0"                   ";"
        "z:/Tools/SREC~1.0/SREC"                                     ";"
        "c:/Program Files (x86)/Beyond Compare 3"                    ";"
        )
      )
    ;; executables path
    (setq profile-exec-path
      '(
         "z:/cmbuilder/3.0beta2_KC390"
         "c:/cygwin/lib/python2.5"
         "d:/CTE/tools/.emacs.d/plugins/gnu_global_628wb/bin"
         "c:/perl/perl/site/bin"
         "c:/perl/perl/bin"
         "c:/perl/c/bin"
         "c:/cygwin/bin"
         "c:/cygwin/usr/bin"
         "c:/cygwin/usr/local/bin"
         "c:/MinGW/msys/1.0/bin"
         "c:/Program Files (x86)/gs/gs9.06/bin"
         "d:/CTE/tools/MikTex/miktex/bin"
         "c:/WINDOWS"
         "c:/WINDOWS/System32"
         "c:/Program Files (x86)/Aspell/bin/"
         "d:/Tools/diab/5.8.0.0/WIN32/bin"
         "c:/Program Files (x86)/IBM/Rational/Synergy/7.1/bin"
         "c:/Program Files (x86)/IBM/Rational/Logiscope/6.6/bin"
         "c:/Program Files (x86)/SYNCHRONe/CCC/3.0"
         "z:/Tools/SREC~1.0/SREC"
         "c:/Program Files (x86)/Beyond Compare 3"
         )
      )
    ;; languages
    (setq profile-lang "en_US") ; for subversion and Dired

    ;; win32 python
    (setenv "PYTHONIOENCODING" "iso8859_15")
    ;; diab compiler
    (setenv "DIABLIB" "d:/Tools/diab/5.8.0.0")
    (setenv "LM_LICENSE_FILE" "d:/Tools/diab/license/WRSLicense.lic")
    ;; cmbuilder
    (setenv "PYTHONPATH" (concat
                           "/cygdrive/z/cmbuilder/3.0beta2_KC390" ":"
                           "/cygdrive/c/cygwin/lib/python2.5"))
    (setenv "cmConfiguration" "Z:/cmbuilder/3.0beta2_KC390/cmConfiguration")
    ) ; (when section-environment-set-path

  ;; MS WINDOWS PERFORMANCE: increase performance on MS Windows
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-ms-windows-performance t)
  ;; EXECUTABLE: set some executable like shell or diff
  ;; REQUIREMENT: `section-environment-os-recognition'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-environment-executable t)
  (when section-environment-executable
    ;; shell
    (setq profile-shell-file-name "C:/cygwin/bin/bash.exe")
    ;; ediff
    (setq profile-ediff-diff-program "D:/CTE/tools/GnuWin32/bin/diff.exe")
    (setq profile-ediff-diff3-program "D:/CTE/tools/GnuWin32/bin/diff3.exe")
    (setq profile-ediff-cmp-program "D:/CTE/tools/GnuWin32/bin/cmp.exe")
    ) ; (when section-environment-executable
  ;; ELPA: packages system support with repositories
  (setq section-environment-elpa t)
  ;; HYPER: menu key become hyper key (modifier key)
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-hyper t)
  ;; SUPER: windows key become super key (modifier key)
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-super nil)
  ;; SERVER : start a server for emacs client (and have only one instance)
  (setq section-environment-server t)
  ) ; (when section-environment


;;; FUNCTIONS: new functions
(setq section-functions t)
(when section-functions
  ;; MAGNETI MARELLI: load custom function for MM profile
  (setq section-function-mm nil)
  ) ; (when section-functions


;;; MODE: load extern files which are modes in plugins/
(setq section-mode t)
(when section-mode
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
  (when section-mode-cedet
    ;; if you want to use emacs included CEDET set to nil
    ;; otherwise set the path of cedet.el and you need to remove:
    ;;    "your-emacs-path/lisp/cedet"
    ;;    "your-emacs-path/lisp/speedbar.*"
    ;;    "your-emacs-path/lisp/emacs-lisp/eieio*"
    (setq profile-cedet-path (concat dotemacs-path "/plugins/cedet-1.1/common/cedet.elc"))
    ;; path of gnu global executable
    (setq profile-gnu-global (concat dotemacs-path "/plugins/gnu_global_628wb/bin/global.exe"))
    ;; path of gnu global executable
    (setq profile-gnu-global-gtags (concat dotemacs-path "/plugins/gnu_global_628wb/bin/gtags.exe"))
    ;;
    ;; SEMANTIC: can do tag, list of function/variable..., preproc, etc
    (setq section-mode-cedet-semantic t)
    (when section-mode-cedet-semantic
      ;; project : the order is important: display in reverse order (first->last)
      (setq profile-ede-project
        '(
           ;; SURDO
;           "y:/SURDO_sw/SURDO.ede.el"
           ;; BANDAS
;           "y:/BANDAS_sw/BANDAS.ede.el"
           ;; TAMBORIM
;           "D:/KC390_BL/Source/TAMBORIM_sw~CTE_BL/TAMBORIM_sw/TAMBORIM.ede.el"
           "D:/KC390/Source/TAMBORIM_sw~FT346598/TAMBORIM_sw/TAMBORIM.ede.el"
           )
        )
      ) ; (when section-mode-cedet-semantic
    ;; ECB: "Emacs Code Browser"
    ;; can display other windows or speedbar to view folder tree, source list,
    ;; variable/function list, buffer history, etc
    (setq section-mode-cedet-ecb t)
    (when section-mode-cedet-ecb
      ;; set default path in "ecb directories"
      (setq profile-ecb-source-path
        '(
           ;; before is put the EDE projects (see project.el)
           ("D:/KC390/Source/KC390_sw~FT346598/KC390_sw"                "KC390_sw")
           ("D:/KC390/Source/GEN_databases~FT346598/GEN_databases"      "GEN_databases")
           ("D:/KC390/Source/Scripts_trace32~FT346598/Scripts_trace32"  "Scripts_trace32")
           ("Z:/"                                                       "KC390_cmBuilder")
           ("C:/Users/FT346598/AppData/Roaming"                         "/home")
           ("C:/cygwin/home/FT346598"                                   "/cygwin")
           ("d:/CTE"                                                    "/CTE")
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
            ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|_ccmwaid\\.inf\\|\\.\\(elc\\|obj\\|o\\|ri2\\|fdc\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|xls\\|doc\\)$\\)\\)")
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
      ) ; (when section-mode-cedet-ecb
    ) ; (when section-mode-cedet
  ;; BATCH: mode for .bat script in MS Windows
  (setq section-mode-batch t)
  ;; VISUAL BASIC: mode for VisualBasic and VisualBasicAdvance
  (setq section-mode-vb t)
  ;; WINDOW NUMBERING: give a number of each window to easily jump in it
  (setq section-mode-window-numbering nil)
  ;; C: define new type in C
  (setq section-mode-c t)
  (when section-mode-c
    ;; CWARN: display small error in source code (forget semi-colon, etc)
    (setq section-mode-c-cwarn nil)
    ;; DATA DEBUG: ??  (not used)
    (setq section-mode-c-data-debug nil)
    ) ; (when section-mode-c
  ;; ICOMPLETION: more completion in Minibuffer
  (setq section-mode-icompletion nil)
  ;; YASNIPPET: snippet mode
  (setq section-mode-yasnippet nil)
  ;; BROWSE KILL RING: mode to browse the kill ring memory yank only on the
  ;; first left top window...
  (setq section-mode-browse-kill-ring t)
  ;; MAGNETI MARELLI:
  (setq section-mode-mm nil)
  (when section-mode-mm
    ;; EOL: syntax color for End Of Line file
    (setq section-mode-mm-eol t)
    ;; CAN DBC: light syntax color for Database CAN file
    (setq section-mode-mm-dbc t)
    ;; CCM DIFF: light syntax color for synergy diff file
    (setq section-mode-mm-diff t)
    ) ; (when section-mode-mm
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
  (when section-mode-vc-clearcase
    ;; path to version tree executable
    (setq profile-clearcase-vtree "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/clearvtree.exe")
    ;; path to cleartool executable
    (setq profile-cleartool "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/cleartool.exe")
    ) ; (when section-mode-vc-clearcase
  ;; CLEARCASE: ClearCase mode
  (setq section-mode-clearcase t)
  (when section-mode-clearcase
    (setq section-mode-clearcase-el nil)
    ) ; (when section-mode-clearcase
  ;; AUTOHOTKEY: AutoHotKey mode
  (setq section-mode-autohotkey t)
  ;; OUTLINE: Outline mode to manually hide/show source code block
  (setq section-mode-outline nil)
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
  ;; CALFW: a more graphical calendar (like google agenda)
  (setq section-mode-calfw nil)
  ;; DIRED DETAILS: show hide details in dired mode
  (setq section-mode-dired-details t)
  ;; SMART TAB: expand or indent at the point with tab
  (setq section-mode-smart-tab nil)
  ;; FOLD DWIM: show hide code source block
  (setq section-mode-fold-dwim t)
  ;; DIRED LETTER ISEARCH: activate by default isearch in dired mode
  (setq section-mode-dired-lis nil)
  ;; NXHTML: enhance html mode
  (setq section-mode-nxhtml nil)
  ;; FASTNAV: fast navigation like with zap-to-char but only to move
  (setq section-mode-fastnav nil)
  ;; MRU YANK: (Most Recently Used) in kill-ring
  (setq section-mode-mru-yank t)
  ;; ACK: search with ack (no more grep) (need perl interpreter)
  (setq section-mode-ack t)
  (when section-mode-ack
    (setq section-mode-ack-full nil)
    (setq section-mode-ack-and-half nil)
    (setq section-mode-ack-emacs t)
    ) ; (when section-mode-ack
  ;; ACE JUMP: move quickly and easily with ace jump
  ;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
  (setq section-mode-ace-jump t)
  ;; DIREDFUL: color dired buffer
  (setq section-mode-diredful nil)
  ;; PS2PDF: print buffer/region in pdf
  (setq section-mode-ps2pdf t)
  ;; AUCTEX: latex mode
  (setq section-mode-auctex t)
  ;; HELM: helm mode (fork of anything mode)
  (setq section-mode-helm t)
  (when section-mode-helm
    ;; replace bookmark list
    (setq section-mode-helm-bookmark t)
    ;; replace electric buffer list
    (setq section-mode-helm-buffers-list t)
    ;; replace browse kill ring
    (setq section-mode-helm-kill-ring t)
    ;; replace M-x
    (setq section-mode-helm-M-x nil)
    ;; replace occur
    (setq section-mode-helm-occur t)
    ;; replace find files C-x C-f
    (setq section-mode-helm-find-files nil)
    ;; replace recentf
    (setq section-mode-helm-recentf t)
    ;; add imenu bind
    (setq section-mode-helm-imenu t)
    ) ; (when section-mode-helm
  ;; YASCROLL: add a small visual scroll-bar (can not be used with mouse click)
  (setq section-mode-yascroll t)
  (when section-mode-yascroll
    ;; time before hide scroll-bar (nil to always show)
    (setq profile-yascroll-delay-to-hide nil)
    ) ; (when section-mode-yascroll
  ;; SMART-FORWARD: move in code with semantic see example in
  ;; plugins/smart-forward.el
  (setq section-mode-smart-forward nil)
  ;; RAINBOW MODE: show string color in color
  (setq section-mode-rainbow t)
  ;; EDIFF: graphical diff (## to toggle whitespace ignoring)
  (setq section-mode-ediff t)
  ;; MAGIT: use git with nice interface (do not use vc interface from emacs)
  (setq section-mode-magit t)
  (when section-mode-magit
    (setq profile-magit-exec "C:/MinGW/msys/1.0/bin/git.exe")
    ) ; (when section-mode-magit
  ;; SYNERGY: use synergy without java client GUI (do not use vc interface from
  ;; emacs)
  (setq section-mode-synergy t)
  (when section-mode-synergy
    (setq profile-synergy-username "FT346598")
    (setq profile-synergy-database "/opt/ccm/databases/kc390_spds")
    (setq profile-synergy-server "http://synergy7.sds.safran:8410")
    (setq profile-synergy-history-filter '(
                                            "|" "sed" "s/F281393/DMN    /"
                                            "|" "sed" "s/FT346530/MQL     /"
                                            "|" "sed" "s/FT346575/MGD     /"
                                            "|" "sed" "s/FT346560/CPT     /"
                                            "|" "sed" "s/FT346433/NGT     /"
                                            "|" "sed" "s/FT346439/BVX     /"
                                            "|" "sed" "s/FT346434/ASE     /"
                                            "|" "sed" "s/FT346581/CCH     /"
                                            "|" "sed" "s/FT346596/MDA     /"
                                            ))
    (setq profile-synergy-diff-external-command "BCompare.exe")
    (setq profile-synergy-diff-external-parameter "")
    (setq profile-synergy-diff-external-swap-file t)
    ) ; (when section-mode-synergy
  ;; DIMINISH: shrink major and minor mode name in the modeline
  (setq section-mode-diminish t)
  ) ; (when section-mode


;;; LANGUAGES:
(setq section-languages t)
(when section-languages
  ;; C: set indentation style and preprocessing option
  ;; REQUIREMENT: profile-c-indent-offset
  (setq section-languages-c t)
  (when section-languages-c
    ;; all profile-c-* variables could be set in ede project
    ;; number of space for indentation in C
    (setq profile-c-indent-offset 4)
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
    ;; prepocessing command
    (setq profile-c-macro-preprocessor "cpp -C")
    ;; compile flags
    (setq profile-c-macro-cppflags "-D__DEBUG__")
    ;; confirme compile command before execute
    (setq profile-c-ask-before-compile t)
    ;;
    ;; INDENT PREPROCESSOR: make a #define be align with C code
    (setq section-languages-c-indent-preprocessor nil)
    (setq section-languages-c-hide-show t)
    (when section-languages-c-hide-show
      (setq section-languages-c-hide-show-hide-all-at-start nil)
      ) ; (when section-languages-c-hide-show
    ) ; (when section-languages-c
  ;; LISP: set indentation style
  (setq section-languages-lisp t)
  (when section-languages-lisp
    ;; number of space for indentation in lisp
    (setq profile-lisp-indent-offset 2)
    ) ; (when section-languages-lisp
  ;; TAB: tab always in space
  (setq section-languages-tabulation t)
  ;; RTRT SCRIPT PTU: set indentation style
  (setq section-languages-rtrt-script t)
  ;; PERL: set indentation style
  (setq section-languages-perl t)
  (when section-languages-perl
    ;; number of space for indentation in perl
    (setq profile-perl-indent-offset 2)
    ) ; (when section-languages-perl
  ) ; (when section-languages


;;; SELECTION: selection can be kill + selection is highlight + kill->copy in
;;; read only
(setq section-selection t)
(when section-selection
  ;; SHIFT SELECTION: selection can be done with shit and arrow keys (default
  ;; setting since 23.3)
  (setq section-selection-with-shift nil)
  ) ; (when section-selection


;;; DISPLAY:
(setq section-display t)
(when section-display
  ;; WINDOWS/BUFFERS: buffers with *buffername* should be displayed in the same
  ;; window first column in window will display buffer limit, next page will leave
  ;; 5 shared line
  (setq section-display-windows-buffers t)
  (when section-display-windows-buffers
    ;;; VISUAL LINE: word wrap, truncate line without cut word
    ;; END and HOME will go to the end/start of screen line not logical line
    (setq section-display-windows-buffers-visual-line nil)
    ) ; (when section-display-windows-buffers
  ;; SPEEDBAR: set size and display of speedbar (see GLOSSARY) (no used)
  (setq section-display-speedbar nil)
  ;; FONT: set font
  ;; REQUIREMENT: `profile-font'
  (setq section-display-font t)
  (when section-display-font
    ;; ANTIALIAS
    ;; set antialiasing on font rendering
;    (setq section-display-font-antialias nil)
    (setq section-display-font-antialias t)
    ;; choice between (it's just some nice font, you can use another font):
    ;;; Terminal
    ;; nice, very tiny, only ascii (too tiny ?)
    ;;(setq profile-font "Terminal-6")
    ;;
    ;;; Anonymous Pro, 10
    ;; nice, big (slashed 'zero', 'one' and minus 'L' can be mixed up)
    ;;(setq profile-font "Anonymous Pro-10")
    ;;; Anonymous Pro, 8
    ;; nice, small (slashed 'zero', 'one' and minus 'L' can be mixed up, parentheses and curly bracket can be mixed up)
    ;;(setq profile-font "Anonymous Pro-8")
    ;;
    ;;; Proggy Tiny Z, 6
    ;; good, very tiny (slashed 'zero', dot and comma can be mixed)
;    (setq profile-font "ProggyTinySZ-6")
    (setq profile-font "Monaco-8")
    ;;
    ;;; DejaVu Sans Mono, 10
    ;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
    ;;(setq profile-font "DejaVu Sans Mono-10")
    ;;; DejaVu Sans Mono, 8
    ;;(setq profile-font "DejaVu Sans Mono-8")
    ;;
    ;;; Inconsolata, 10
    ;; not so good with ms window (slashed 'zero', capitalized 'i' and minus 'L' can be mixed up)
    ;;(setq profile-font "Inconsolata-10")
    ;;
    ;;; Lucida Console, 10
    ;; nice, big, large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
    ;;(setq profile-font "Lucida Console-10")
    ;;; Lucida Console, 8
    ;; nice, small large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
    ;;(setq profile-font "Lucida Console-8")
    ;;
    ;;; Monaco, 10
    ;; nice, very big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
    ;;(setq profile-font "Monaco-10")
    ;;; Monaco, 8
    ;; nice, big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
    ;;(setq profile-font "Monaco-8")
    ;;
    ;;; ProFont, 8
    ;; nice, tiny, (slashed 'zero', 'one' and minus 'L' can be mixed up)
    ;;(setq profile-font "ProFontWindows-8")
    ;;
    ;;; Courier New 10
    ;; classic but big and large
    ;;(setq profile-font "Courier New-10")
    ;;; Courier New 8
    ;; classic but big and large
    ;;(setq profile-font "Courier New-8")
    ;;
    ;; INTERNATIONAL: ISO or utf-8 or ...  (not used)
    (setq section-display-font-international t)
    ) ; (when section-display-font
  ;; COLOR: set color
  (setq section-display-color t)
  (when section-display-color
    ;; PARENTHESES MODE: matched parentheses are highlight
    (setq section-display-color-parentheses-mode t)
    ;; PARENTHESES MINIBUFFER: matched parentheses are highlight and if not
    ;; visible show it in the Minibuffer
    (setq section-display-color-parentheses-visible t)
    ;; PARENTHESES HIGHLIGHT: matched parentheses are highlight in rainbow color
    (setq section-display-color-parentheses-highlight nil)
    ;; COLOR THEME: set color by color-theme mode (or manual settings nil)
    (setq section-display-color-theme t)
    (if  section-display-color-theme
      ;; do not use it with terminal
      ;; theme to be used
      (setq profile-color-theme "solarized-light")
      (progn ; if section-display-color-theme nil
        ;;
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
      ) ; (if  section-display-color-theme
    ;; ANSI COLOR COMPILE WINDOW: have color and no more junk like this ^[[32m
    (setq section-display-color-ansi-color-compile t)
    ) ; (when section-display-color
  ) ; (when section-display


;;; INTERFACE: display buffer name in titlebar (example "<[ foobar.c ]>")
(setq section-interface t)
(when section-interface
  ;; DECORATION: remove all mouse interface (toolbar, menubar, scrollbar)
  (setq section-interface-remove-decoration t)
  ;; MODELINE: set some option to add in the grey line at the bottom of each
  ;; buffer
  (setq section-interface-modeline t)
  ;; WINDOW TITLE
  ;; %b buffername ; %F frame name ; %l line number ; %c column number
  ;; %p percent of buffer above top ; %m mode name ; %n Narrow mode
  ;; %z coding systems ; %Z %z + end-of-line format ; %- infinitely dashes
  (setq profile-window-title "GNU Emacs <[ %b ]>")
  ;; TRANSPARENCY: the whole emacs will be transparent
  ;; REQUIREMENT: `profile-transparency'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-interface-transparency nil)
  (when section-interface-transparency
    ;; transparency of the window. 0=transparent/100=opaque
    (setq profile-transparency 96)
    ) ; (when section-interface-transparency
  ;; FULLSCREEN:
  ;; REQUIREMENT: `section-environment-os-recognition'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-interface-fullscreen t)
  ;; ECB: set size, display, refresh and remove opening tips
  ;; REQUIREMENT: `section-mode-cedet-ecb'
  (setq section-interface-ecb t)
  (when section-interface-ecb
    ;; ECB ASCII TREE: display ascii guides instead of image for arborescence
    ;; tree
    (setq section-interface-ecb-ascii-tree t)
    ) ; (when section-interface-ecb
  ) ; (when section-interface


;;; COMPLETION: enable letter case completion + dynamic completion
(setq section-completion t)


;;; SHORTCUT:
(setq section-shortcut t)
(when section-shortcut
  ;; ALL (GLOBAL): add global shortcut (for whole Emacs)
  (setq section-shortcut-global t)
  (when section-shortcut-global
    ;; CUA: enable C-x, C-c, C-v to cut copy paste
    ;; don't recommend it otherwise see http://www.emacswiki.org/CuaMode
    (setq section-shortcut-global-cua nil)
    ) ; (when section-shortcut-global
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
  (when section-shortcut-tags
    ;; ETAGS:
    (setq section-shortcut-tags-exuberant-ctags nil)
    ;; GTAGS:
    ;; REQUIREMENT: section-mode-gnu-global
    (setq section-shortcut-tags-gnu-global t)
    ) ; (when section-shortcut-tags
  ;; SEMANTIC: add shortcut to move in source code with semantic
  ;; REQUIREMENT: `section-mode-cedet-semantic'
  (setq section-shortcut-semantic t)
  ) ; (when section-shortcut


;;; MOUSE: smooth wheel + lazy decoration when scroll
(setq section-mouse t)
(when section-mouse
  ;; PASTE CURSOR: yank at point not mouse cursor (either when yank with mouse
  ;; wheel)
  (setq section-mouse-paste-to-point-not-mouse-cursor t)
  ;; AVOIDANCE: mouse cursor avoid the keyboard cursor when typing
  ;; REQUIREMENT: `section-environment-terminal-vs-graphics'
  (setq section-mouse-avoidance t)
  ;; SMOOTH SCROLL: it will always scroll line by line with arrow at start or
  ;; end of screen
  (setq section-mouse-smooth-scroll nil)
  ) ; (when section-mouse


;;; ANNOYANCES: no welcome message + yes->y + do not query to refresh buffer +
;; remove insert key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog
;; box + no tooltips
(setq section-annoyances t)
(when section-annoyances
  ;; TRUNCATE LINE: whole line not visible (need to scroll right)
  (setq section-annoyances-truncate-line nil)
  ;; SCROLL PRESERVE CURSOR POSITION: when wheel scroll the cursor do not move
  (setq section-annoyances-scroll-preserve-cursor-position nil)
  ;; NO BACKUP FILE: no backup file will be created
  (setq section-annoyances-no-backup-file nil)
  ;; ALL BACKUP FILE IN DIRECTORY all backup files will be created in a
  ;; directory
  (setq section-annoyances-backup-file-in-directory t)
  ) ; (when section-annoyances


;;; MISC: remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
;; REQUIREMENT: `profile-username'
;;              `profile-column'
(setq section-misc t)
(when section-misc
  ;; SPACE
  (setq profile-remove-useless-ending-space t)
  (setq profile-always-new-line-at-end t)
  ;; backup directory
  (setq profile-backup-directory "D:/tmp/emacs")
  (setq profile-autosave-directory "D:/tmp/emacs")
  ;; use by fill-xxx or fill column indicator mode
  (setq profile-fill-column 80)
  ;; browser to open url
  (setq profile-browser "D:/CTE/tools/OperaNext/launcher.exe")
  ;;
  ;; CALENDAR set latitude/longitude + location + holidays + custom date in
  ;; Modeline lunar phase, sunrise/sunset, time etc
  ;; REQUIREMENT: profile-longitude
  ;;              profile-latitude
  ;;              profile-location-name
  (setq section-misc-calendar t)
  (when section-misc-calendar
    ;; CALENDAR in French
    (setq section-misc-calendar-french t)
    ) ; (when section-misc-calendar
  ;; DICTIONARY: set default dictionary, etc
  ;; REQUIREMENT: `profile-ispell-program'
  ;;              `profile-ispell-dictionary'
  (setq section-misc-dictionary t)
  (when section-misc-dictionary
    ;; path of apsell
    (setq profile-ispell-program "aspell")
    ;; default dictionnary
    (setq profile-ispell-dictionary "english")
    ) ; (when section-misc-dictionary
  ;; BOOKMARK: set default bookmark storage
  (setq section-misc-bookmark t)
  (when section-misc-bookmark
    ;; sort bookmark
    (setq profile-bookmark-sort nil)
    ) ; (when section-misc-bookmark
  ;; SCREENSAVER: Set screensaver when idle time higher than 5 minutes
  (setq section-misc-screensaver nil)
  ) ; (when section-misc


;;; CUSTOMIZE: all customize settings are put in here when you use interface
;; (customize) to change settings
(setq section-filecustomize t)

;;; AFTER LOADING CONF
;; this function will be call at the end after all configuration, it can be use
;; to override some settings or add settings without modify the configuration
(defun function-to-call-after-loading-conf ()
  ;; to disable all version control
  ;(setq vc-handled-backends nil)

  ;; set only one zone program
    (setq zone-programs [ zone-pgm-putz-with-case ])
  ) ; (defun function-to-call-after-loading-conf ()

(provide 'profile-sagem-defense-light)

;;; profile-sagem-defense-light.el ends here