;;; profile-xubuntu.el --- a config file for profile

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
;; Version: 0.5
;; Created: June 2012
;; Last-Updated: December 2012

;;; Commentary:
;;
;; load by `dotemacs/profile.el'
;; REQUIREMENT: var     `section-environment-profile'

;;; Change Log:
;; 2012-12-05 (0.5)
;;    reorganize the file (like emacs.el)
;; 2012-06-26 (0.4)
;;    simplify font settings
;; 2012-06-13 (0.3)
;;    new modes
;; 2012-06-12 (0.2)
;;    add new section and option
;; 2012-06-04 (0.1)
;;    creation from scratch


;;; Code:
;; load private variable
(try-require 'profile-xubuntu-private "    ")

;; Enable/disable section:
;; (setq section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' (disable) or 't' (enable) value
;;
;; Settings:
;; (setq profile-xxx X)

;;; ENVIRONMENT
(setq section-environment t)
(progn
  ;; PROFILE: load profile from .emacs.d/profile/ directory
  (setq section-environment-profile t)
  (progn
    (setq profile "xubuntu")
    ) ; (progn
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
  (progn
    ;; path
    (setq profile-path
      (concat
        "/bin" ":"
        )
      )
    ;; executables path
    (setq profile-exec-path
      '(
         "/bin"
         )
      )
    ;; languages
    (setq profile-lang "en_US") ; for subversion and Dired
    ) ; (progn
  ;; MS WINDOWS PERFORMANCE: increase performance on MS Windows
  ;; REQUIREMENT: `section-environment-os-recognition'
  (setq section-environment-ms-windows-performance t)
  ;; EXECUTABLE: set some executable like shell or diff
  ;; REQUIREMENT: `section-environment-os-recognition'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-environment-executable t)
  (progn
    ;; shell
    (setq profile-shell-file-name "/bin/bash")
    ;; ediff
    (setq profile-ediff-diff-program "/bin/diff")
    (setq profile-ediff-diff3-program "/bin/diff3")
    (setq profile-ediff-cmp-program "/bin/cmp")
    ) ; (progn
  ;; ELPA: packages system support with repositories
  (setq section-environment-elpa nil)
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
    ;; if you want to use emacs included CEDET set to nil
    ;; otherwise set the path of cedet.el and you need to remove:
    ;;    "your-emacs-path/lisp/cedet"
    ;;    "your-emacs-path/lisp/speedbar.*"
    ;;    "your-emacs-path/lisp/emacs-lisp/eieio*"
    (setq profile-cedet-path (concat dotemacs-path "/plugins/cedet-1.1/common/cedet.elc"))
    ;; path of gnu global executable
    (setq profile-gnu-global "/bin/global")
    ;; path of gnu global executable
    (setq profile-gnu-global-gtags "/bin/gtags")
    ;;
    ;; SEMANTIC: can do tag, list of function/variable..., preproc, etc
    (setq section-mode-cedet-semantic t)
    (progn
      ;; project : the order is important: display in reverse order (first->last)
      (setq profile-ede-project
        '(
           ;; project
           ;;"/path/to/project/project.ede.el"
           )
        )
      ) ; (progn
    ;; ECB: "Emacs Code Browser"
    ;; can display other windows or speedbar to view folder tree, source list,
    ;; variable/function list, buffer history, etc
    (setq section-mode-cedet-ecb t)
    (progn
      ;; set default path in "ecb directories"
      (setq profile-ecb-source-path
        '(
           ;; before is put the ede projects (see project.el)
           ("~/.emacs.d/"        "Dotemacs")
           )
        )
      ;; regexp of folder to exclude in "ecb directories"
      (setq profile-ecb-excluded-directories-regexps
        '(
           "^\\.+$"             ; hidden folder
           "\\(TOTO\\|TITI\\)$" ; example
           )
        )
      ;; files to be ignored in "ecb source" !! RTFM !!
      (setq profile-ecb-source-file-regexps
        '((".*"
            ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\)$\\)\\)")
            ("^\\.\\(emacs\\|gnus\\)$"))
           )
        )
      ;; files to be ignored from Version Control VC
      (setq profile-ecb-sources-exclude-cvsignore
        '(
           "example"
           )
        )
      ;; regexp to form group in "ecb history"
      (setq profile-ecb-history-make-buckets
        '(
           "include" ; all file from an include folder
           "\\.[hc][p]*$" ; all c cpp and h files
           "\\.el$" ; all elisp files
           )
        )
      ) ; (progn
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
  (setq section-mode-yasnippet t)
  ;; BROWSE KILL RING: mode to browse the kill ring memory yank only on the
  ;; first left top window...
  (setq section-mode-browse-kill-ring t)
  ;; MAGNETI MARELLI:
  (setq section-mode-mm nil)
  (progn
    ;; EOL: syntax color for End Of Line file
    (setq section-mode-mm-eol nil)
    ;; CAN DBC: light syntax color for Database CAN file
    (setq section-mode-mm-dbc nil)
    ;; CCM DIFF: light syntax color for synergy diff file
    (setq section-mode-mm-diff nil)
    ) ; (progn
  ;; DIRED+: improve Dired mode, color, open with, etc
  (setq section-mode-dired-plus t)
  ;; GNU/GLOBAL: Tag management mode (use modified gtags.el)
  (setq section-mode-gnu-global t)
  ;; EPROJECT (grischka): project management mode (never used)
  (setq section-mode-eproject nil)
  ;; RTRT SCRIPT: rtrt script mode (syntax coloration)
  (setq section-mode-rtrt-script nil)
  ;; VC CLEARCASE: vc ClearCase mode
  ;; REQUIREMENT: `profile-clearcase-vtree'
  ;;              `profile-cleartool'
  (setq section-mode-vc-clearcase nil)
  (progn
    ;; path to version tree executable
    (setq profile-backup-directory (concat dotemacs-path "/backup"))
    ;; path to cleartool executable
    (setq profile-autosave-directory (concat dotemacs-path "/cache"))
    ) ; (progn
  ;; CLEARCASE: ClearCase mode (not used)
  (setq section-mode-clearcase nil)
  (progn
    (setq section-mode-clearcase-el nil)
    ) ; (progn
  ;; AUTOHOTKEY: AutoHotKey mode
  (setq section-mode-autohotkey nil)
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
  (setq section-mode-subversion t)
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
  ;; ACE JUMP: move quickly and easily with ace jump
  ;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
  (setq section-mode-ace-jump t)
  ;; DIREDFUL: color dired buffer
  (setq section-mode-diredful t)
  ;; PS2PDF: print buffer/region in pdf
  (setq section-mode-ps2pdf t)
  ;; AUCTEX: latex mode
  (setq section-mode-auctex nil)
  ;; HELM: helm mode (fork of anything mode)
  (setq section-mode-helm nil)
  (progn
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
    (setq section-mode-helm-find-files t)
    ;; replace recentf
    (setq section-mode-helm-recentf t)
    ;; add imenu bind
    (setq section-mode-helm-imenu t)
    ) ; (progn
  ;; DIMINISH: shrink major and minor mode name in the modeline
  (setq section-mode-diminish nil)
  ) ; (progn


;;; LANGUAGES:
(setq section-languages t)
(progn
  ;; C: set indentation style and preprocessing option
  ;; REQUIREMENT: profile-c-indent-offset
  (setq section-languages-c t)
  (progn
    ;; all profile-c-* variables could be set in ede project
    ;; number of space for indentation in C
    (setq profile-c-indent-offset 2)
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
    (progn
      (setq section-languages-c-hide-show-hide-all-at-start nil)
      ) ; (progn
    ) ; (progn
  ;; LISP: set indentation style
  (setq section-languages-lisp t)
  (progn
    ;; number of space for indentation in lisp
    (setq profile-lisp-indent-offset 2)
    ) ; (progn
  ;; TAB: tab always in space
  (setq section-languages-tabulation t)
  ;; RTRT SCRIPT PTU: set indentation style
  (setq section-languages-rtrt-script nil)
  ;; PERL: set indentation style
  (setq section-languages-perl t)
  (progn
    ;; number of space for indentation in perl
    (setq profile-perl-indent-offset 2)
    ) ; (progn
  ) ; (progn


;;; SELECTION: selection can be kill + selection is highlight + kill->copy in
;;; read only
(setq section-selection t)
(progn
  ;; SHIFT SELECTION: selection can be done with shit and arrow keys (default
  ;; setting since 23.3)
  (setq section-selection-with-shift t)
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
    ;; choice between (it's just some nice font, you can use another font):
    ;;; Terminal
    ;; nice, very tiny, only ascii (too tiny ?)
    ;; (setq profile-font "Terminal-6")
    ;;
    ;;; Anonymous Pro, 10
    ;; nice, big (slashed 'zero', 'one' and minus 'L' can be mixed up)
    ;;(setq profile-font "Anonymous Pro-10")
    ;;; Anonymous Pro, 8
    ;; nice, small (slashed 'zero', 'one' and minus 'L' can be mixed up, parentheses and curly bracket can be mixed up)
    ;; (setq profile-font "Anonymous Pro-8")
    ;;
    ;;; Proggy Tiny Z, 6
    ;; good, very tiny (slashed 'zero', dot and comma can be mixed)
    ;; (setq profile-font "ProggyTinySZ-6")
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
    ;;
    ;;; Ubuntu Mono, 10
    ;; (setq profile-font "Ubuntu Mono-10")
    ;;; Ubuntu Mono, 8
    (setq profile-font "Ubuntu Mono-8")
    ;;
    ;;; Terminus, 10
    ;; (setq profile-font "Terminus-10")
    ;;; Terminus, 8
    ;; (setq profile-font "Terminus-8")
    ;;
    ;;; Monospace, 8
    ;; (setq profile-font "Monospace-8")
    ;;
    ;;; Liberation Mono, 8
    ;; (setq profile-font "Liberation Mono-8")
    ;;
    ;;; Inconsolata, 10
    ;; (setq profile-font "Inconsolata-10")
    ;;; Inconsolata, 8
    ;; (setq profile-font "Inconsolata-8")
    ;;
    ;;; Droid Sans Mono, 10
    ;; (setq profile-font "Droid Sans Mono-10")
    ;;; Droid Sans Mono, 8
    ;; (setq profile-font "Droid Sans Mono-8")
    ;;
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
      ;; theme to be used
      (setq profile-color-theme "zenburn")
      ;;
      ;; MISC: current line highlight + full syntax coloration
      (setq section-display-color-misc nil)
      ;; MODE: set color for c-mode, cursor and current line
      (setq section-display-color-mode nil)
      ;; GREP: set color for grep window (all search, occur, grep, grep-find,
      ;; etc)
      (setq section-display-color-grep nil)
      ;; ECB: set color for ecb-mode
      ;; REQUIREMENT: `section-mode-cedet-ecb'
      (setq section-display-color-ecb nil)
      ) ; (progn
    ) ; (progn
  ) ; (progn


;;; INTERFACE: display buffer name in titlebar (example "<[ foobar.c ]>")
(setq section-interface t)
(progn
  ;; DECORATION: remove all mouse interface (toolbar, menubar, scrollbar)
  (setq section-interface-remove-decoration nil)
  ;; MODELINE: set some option to add in the grey line at the bottom of each
  ;; buffer
  (setq section-interface-modeline t)
  ;; TRANSPARENCY: the whole emacs will be transparent
  ;; REQUIREMENT: `profile-transparency'
  ;;              `section-environment-terminal-vs-graphics'
  (setq section-interface-transparency t)
  (progn
    ;; transparency of the window. 0=transparent/100=opaque
    (setq profile-transparency 96)
    ) ; (progn
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
    (setq section-interface-ecb-ascii-tree nil)
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
  (setq section-mouse-paste-to-point-not-mouse-cursor nil)
  ;; AVOIDANCE: mouse cursor avoid the keyboard cursor when typing
  ;; REQUIREMENT: `section-environment-terminal-vs-graphics'
  (setq section-mouse-avoidance nil)
) ; (progn


;;; ANNOYANCES: no welcome message + yes->y + do not query to refresh buffer +
;; remove insert key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog
;; box + no tooltips
(setq section-annoyances t)
(progn
  ;; TRUNCATE LINE: whole line not visible (need to scroll right)
  (setq section-annoyances-truncate-line t)
  ;; SCROLL PRESERVE CURSOR POSITION: when wheel scroll the cursor do not move
  (setq section-annoyances-scroll-preserve-cursor-position t)
  ;; NO BACKUP FILE: no backup file will be created
  (setq section-annoyances-no-backup-file nil)
  ;; ALL BACKUP FILE IN DIRECTORY all backup files will be created in a
  ;; directory
  (setq section-annoyances-backup-file-in-directory t)
  ;; CLASSIC SCROLL: when scroll at start or end screen with arrow, it will
  ;; always scroll line by line
  (setq section-annoyances-classic-scroll t)
  ) ; (progn


;;; MISC: remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
;; REQUIREMENT: `profile-username'
;;              `profile-column'
(setq section-misc t)
(progn
  ;; backup directory
  (setq profile-backup-directory (concat dotemacs-path "/backup"))
  (setq profile-autosave-directory (concat dotemacs-path "/cache"))
  ;; use by fill-xxx or fill column indicator mode
  (setq profile-fill-column 80)
  ;; browser to open url
  (setq profile-browser "firefox")
  ;;
  ;; CALENDAR set latitude/longitude + location + holidays + custom date in
  ;; Modeline lunar phase, sunrise/sunset, time etc
  ;; REQUIREMENT: profile-longitude
  ;;              profile-latitude
  ;;              profile-location-name
  (setq section-misc-calendar nil)
  (progn
    ;; CALENDAR in French
    (setq section-misc-calendar-french t)
    ) ; (progn
  ;; DICTIONARY: set default dictionary, etc
  ;; REQUIREMENT: `profile-ispell-program'
  ;;              `profile-ispell-dictionary'
  (setq section-misc-dictionary nil)
  (progn
    ;; path of apsell
    (setq profile-ispell-program "aspell")
    ;; default dictionnary
    (setq profile-ispell-dictionary "english")
    ) ; (progn
  ;; BOOKMARK: set default bookmark storage
  (setq section-misc-bookmark t)
  ;; SCREENSAVER: Set screensaver when idle time higher than 5 minutes
  (setq section-misc-screensaver t)
  ) ; (progn


;;; CUSTOMIZE: all customize settings are put in here when you use interface
;; (customize) to change settings
(setq section-filecustomize t)

;;; AFTER LOADING CONF
;; this function will be call at the end after all configuration, it can be use
;; to override some settings or add settings without modify the configuration
(defun function-to-call-after-loading-conf ()
  ) ; (defun function-to-call-after-loading-conf ()


(provide 'profile-xubuntu)

;;; profile-xubuntu.el ends here