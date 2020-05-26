;;; init-profile.el --- a config file for profile

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.6
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;

;;; Change Log:
;; 2020-04-05 (0.6)
;;    add use-package
;; 2018-01-31 (0.5)
;;    add ada function, ripgrep, hydra ada, ada mode, pandoc, fitnesse, flex isearch + reuse/adapt
;;    fill column indicator
;; 2017-09-28 (0.4)
;;    add dumb jump mode
;; 2017-09-12 (0.3)
;;    add helm global mode setting
;; 2017-09-11 (0.2)
;;    update to new mode and small optimization
;; 2017-07-07 (0.1)
;;    creation from scratch

;;; Code:

;; ENVIRONMENT: Environment check and configuration
(defvar tqnr-section-environment nil)
(progn ;; tqnr-section-environment
  ;; GARBAGE COLLECTION: speed up start of emacs
  (defvar tqnr-section-environment-garbage-collection nil)
  (progn ;; tqnr-section-environment-garbage-collection
    ;; MINIBUFFER: increase temporarily garbage collection when execute something
    (defvar tqnr-section-environment-garbage-collection-minibuffer nil)
    ;; START: speed up a little the start of emacs
    (defvar tqnr-environment-garbage-collection-start nil)
    ) ;; (progn ;; tqnr-section-environment-garbage-collection
  ;; VERSION RECOGNITION: detect Emacs version
  (defvar tqnr-section-environment-version-recognition nil)
  ;; OS RECOGNITION: detect OS type (MS Windows vs GNU Linux)
  (defvar tqnr-section-environment-os-recognition nil)
  ;; TERMINAL VS GRAPHICS: detect Emacs terminal vs graphics
  (defvar tqnr-section-environment-terminal-vs-graphics nil)
  ;; SET PATH: Set environment variable PATH
  (defvar tqnr-section-environment-set-path nil)
  (progn ;; tqnr-section-environment-set-path
    ;; PATH environment variable concat with current PATH
    (defvar tqnr-profile-path '())
    ;;
    ;; LOCALE: languages settings about subversion and dired
    (defvar tqnr-profile-lang "")
    ) ;; (progn ;; tqnr-section-environment-set-path
  ;; MS WINDOWS PERFORMANCE: MS Windows specific configuration about performance
  (defvar tqnr-section-environment-ms-windows-performance nil)
  ;; EXECUTABLE: Set path of some exe
  (defvar tqnr-section-environment-executable nil)
  (progn ;; tqnr-section-environment-executable
    ;; diff program
    (defvar tqnr-profile-ediff-diff-program "")
    (defvar tqnr-profile-ediff-diff3-program "")
    (defvar tqnr-profile-ediff-cmp-program "")
    ) ;; (progn ;; tqnr-section-environment-executable
  ;; HYPER: Enable Hyper modifier key (Menu key, shortcut with "H-") on MS Windows
  (defvar tqnr-section-environment-hyper nil)
  ;; SUPER: Enable Super modifier key (Windows key, shortcut with "s-") on MS Windows
  (defvar tqnr-section-environment-super nil)
  ;; SERVER: start the emacs server to have only one emacs client instance
  (defvar tqnr-section-environment-server nil)

  ;; PACKAGE: package management
  (defvar tqnr-section-environment-package nil)
  (progn ;; tqnr-section-environment-package
    ;; PROXY: proxy setting about package management
    (defvar tqnr-profile-environment-package-proxy-http nil)
    (defvar tqnr-profile-environment-package-proxy-https nil)
    ;; LOCAL PATH: proxy setting about package management
    (defvar tqnr-profile-environment-package-local-path nil)
    ;; PACKAGE LIST: list of package like "'(first-package second-package)" to be installed
    (defvar tqnr-profile-environment-package-list '())
    ) ;; (progn ;; tqnr-section-environment-package

  ;; SHORTCUT: Environment shortcut to declare hook about shortcut
  (defvar tqnr-section-environment-shortcut nil)
  ) ;; (progn ;; tqnr-section-environment

;; FUNCTION: custom functions
(defvar tqnr-section-function nil)
(progn ;; tqnr-section-function

  ;; GENERATE PROFILE
  ;; functions to generate profile from all config file
  ;;
  ;; interactive function `tqnr-generate-profile' will parse dotemacs directory to
  ;; generate a default profile file or with a parameter not nil init-profile.el file
  (defvar tqnr-section-function-generate-profile nil)

  ;; EDIT BUFFER: custom function about edition of buffer text
  (defvar tqnr-section-function-edit-buffer nil)
  (progn ;; tqnr-section-function-edit-buffer
    ;; FLAG DEBUG: used in kaneton project (epita)
    (defvar tqnr-function-kaneton nil)
    ) ;; (progn ;; tqnr-section-function-edit-buffer

  ;; SELECT COPY: custom function about copying and selecting buffer text
  (defvar tqnr-section-function-select-copy nil)

  ;; SEARCH: custom function about searching in buffer text
  (defvar tqnr-section-function-search nil)

  ;; BUFFER WINDOW: custom function about buffer and window handling
  (defvar tqnr-section-function-buffer-window nil)

  ;; FILE: custom function about file management
  (defvar tqnr-section-function-file nil)

  ;; MACRO: custom function about macro management
  (defvar tqnr-section-function-macro nil)

  ;; SEMANTIC: custom function about semantic mode
  (defvar tqnr-section-function-semantic nil)

  ;; WEB: custom function about web interface to browser
  (defvar tqnr-section-function-web nil)

  ;; HELM: custom function about Helm mode
  (defvar tqnr-section-function-helm nil)

  ;; AUCTEX: custom function about AUCTeX
  (defvar tqnr-section-function-auctex nil)

  ;; ORG MODE: custom function about org-mode
  (defvar tqnr-section-function-org-mode nil)

  ;; MAGNETI MARELLI: custom functions about Magneti Marelli specific needs
  (defvar tqnr-section-function-magneti-marelli nil)

  ;; ECB: custom function about ecb mode
  (defvar tqnr-section-function-ecb nil)

  ;; CLEARCASE: custom function about ClearCase integration into GNU Emacs
  (defvar tqnr-section-function-clearcase nil)

  ;; RTRT: custom function about RTRT script .ptu file
  (defvar tqnr-section-function-rtrt nil)

  ;; ADA: functions to have multiple compile mode in ada
  (defvar tqnr-section-function-ada nil)
  (progn ;; tqnr-section-function-ada
    ;; BUILD
    ;; Command of build one file in ada
    (defvar tqnr-ada-gps-build-command "gnat build")
    ;; Buffer name of build one file in ada
    (defvar tqnr-ada-gps-build-buffer-name "*ada-build*")
    ;; CHECK
    ;; Command of check one file in ada
    (defvar tqnr-ada-gps-check-command "gnat check")
    ;; Buffer name of check one file in ada
    (defvar tqnr-ada-gps-check-buffer-name "*ada-check*")
    ;; PRETTY PRINT
    ;; Command of pretty-print one file in ada
    (defvar tqnr-ada-gps-pretty-print-command "gnat pretty")
    ;; Buffer name of pretty-print one file in ada
    (defvar tqnr-ada-gps-pretty-print-buffer-name "*ada-pretty-print*")
    ;; BUILD ALL
    ;; Command of build all project files
    (defvar tqnr-ada-gps-build-all-command "gnat make")
    ;; Buffer name of build all project files command
    (defvar tqnr-ada-gps-build-all-buffer-name "*ada-build-all*")
    ;; CLEAN
    ;; Command of clean all generated file
    (defvar tqnr-ada-gps-clean-command "gnat clean")
    ;; Buffer name of clean all generated command
    (defvar tqnr-ada-gps-clean-buffer-name "*ada-clean*")
    ;; REBUILD ALL
    ;; Command of rebuild all generated file
    (defvar tqnr-ada-gps-rebuild-all-command "gnat clean && gnat make")
    ;; Buffer name of rebuild all generated command
    (defvar tqnr-ada-gps-rebuild-all-buffer-name "*ada-rebuild-all*")
    ;; BUILD NATIVE
    ;; Command of build for native execution
    (defvar tqnr-ada-gps-build-native-command "gnat clean")
    ;; Buffer name of native build command
    (defvar tqnr-ada-gps-build-native-buffer-name "*ada-build-native*")
    ) ;; (progn ;; tqnr-section-function-ada

  ;; CALC: functions to add type support to helm ag for ripgrep
  (defvar tqnr-section-function-calc nil)
  ) ;; (progn ;; tqnr-section-function

;; MODE: load extern files which are modes in plugins/
(defvar tqnr-section-mode nil)
(progn ;; tqnr-section-mode

  ;; USE PACKAGE: Package configuration with simple and tidy macro
  (defvar tqnr-section-mode-use-package nil)

  ;; HELM: (fork ANYTHING) choose anything with the same nice interface
  (defvar tqnr-section-mode-helm nil)
  (progn ;; tqnr-section-mode-helm
    ;; isearch by helm
    (defvar tqnr-section-mode-helm-swoop nil)
    ;; do not have default value when run helm swoop
    (defvar tqnr-section-mode-helm-swoop-without-pre-input nil)
    ;; replace fuzzy search in find-files by flx, more human matches
    (defvar tqnr-section-mode-helm-flx nil)
    ;; replace yank-pop or browse kill ring by helm-kill-ring
    (defvar tqnr-section-mode-helm-kill-ring nil)
    ;; replace M-x
    (defvar tqnr-section-mode-helm-M-x nil)
    ;; replace electric buffer list
    (defvar tqnr-section-mode-helm-buffers-list nil)
    ;; add helm menu shortcut for function/variable list
    (defvar tqnr-section-mode-helm-imenu nil)
    ;; replace occur by helm
    (defvar tqnr-section-mode-helm-occur nil)
    ;; replace find files C-x C-f
    (defvar tqnr-section-mode-helm-find-files nil)
    ;; replace recentf
    (defvar tqnr-section-mode-helm-recentf nil)
    ;; replace bookmark list
    (defvar tqnr-section-mode-helm-bookmark nil)
    ;; enable Helm everywhere when asking file
    (defvar tqnr-section-mode-helm-global nil)
    ) ;; (progn ;; tqnr-section-mode-helm

  ;; HOME END: add some useful function to home and end keys
  (defvar tqnr-section-mode-home-end nil)

  ;; DOXYMACS: emacs interface for doxygen comments
  (defvar tqnr-section-mode-doxymacs nil)

  ;; IDO
  ;; yet another switch buffer
  ;; 2010: erratic behavior with exotic filename under MS Windows (not used)
  (defvar tqnr-section-mode-ido nil)

  ;; UNIQUIFY: create unique buffer names
  (defvar tqnr-section-mode-uniquify nil)

  ;; CEDET: "Collection of Emacs Development Environment Tools"
  (defvar tqnr-section-mode-cedet nil)
  (progn ;; tqnr-section-mode-cedet
    ;; bin path of gnu global for cedet
    (defvar tqnr-profile-gnu-global (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_656wb/bin/global.exe"))
    (defvar tqnr-profile-gnu-global-gtags (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_656wb/bin/gtags.exe"))
    ;;
    ;; CEDET SEMANTIC: can do tag, list of function/variable..., preproc, etc
    (defvar tqnr-section-mode-cedet-semantic nil)
    ;; list of projects: the order is important, display in reverse order (first->last)
    (defvar tqnr-profile-ede-project '())
    ;;
    ;; CEDET ECB (Emacs Code Browser): transform Emacs interface to IDE
    ;; can display other windows or speedbar to view folder tree, source list,
    ;; variable/function list, buffer history, etc
    (defvar tqnr-section-mode-cedet-ecb nil)
    ;; set default path in "ecb directories"
    (defvar tqnr-profile-ecb-source-path '())
    ;; regexp of folder to exclude in "ecb directories"
    (defvar tqnr-profile-ecb-excluded-directories-regexps '())
    ;; files to be ignored in "ecb source" !! RTFM !!
    (defvar tqnr-profile-ecb-source-file-regexps '(()))
    ;; files to be ignored from Version Control VC
    (defvar tqnr-profile-ecb-sources-exclude-cvsignore '())
    ;; regexp to form group in "ecb history"
    (defvar tqnr-profile-ecb-history-make-buckets '())
    ) ;; (progn ;; tqnr-section-mode-cedet

  ;; BATCH: mode for .bat script in MS Windows
  (defvar tqnr-section-mode-batch nil)

  ;; VISUAL BASIC: mode for VisualBasic and VisualBasicAdvance
  (defvar tqnr-section-mode-visual-basic nil)

  ;; WINDOW NUMBERING: give a number of each window to easily jump in it
  (defvar tqnr-section-mode-window-numbering nil)

  ;; CWARN
  ;; show small warning in code source
  ;; (ex: set in test, semi colon after test...)
  (defvar tqnr-section-mode-cwarn nil)

  ;; C DATA DEBUG: ?? (never manage to make it work)
  (defvar tqnr-section-mode-c-data-debug nil)

  ;; ICOMPLETION: more completion in minibuffer
  (defvar tqnr-section-mode-icompletion nil)

  ;; YASNIPPET: enable snippet for emacs
  (defvar tqnr-section-mode-yasnippet nil)

  ;; BROWSE KILL RING: mode to browse the kill ring memory yank only on the first left top window...
  (defvar tqnr-section-mode-browse-kill-ring nil)

  ;; MM EOL: MAGNETI MARELLI, End Of Line file mode
  (defvar tqnr-section-mode-mm-eol nil)

  ;; MM DBC: CAN dbc mode
  (defvar tqnr-section-mode-mm-dbc nil)

  ;; MM DIFF: synergy classic diff mode
  (defvar tqnr-section-mode-mm-diff nil)

  ;; DIRED PLUS: improve Dired mode, color, open with, etc
  (defvar tqnr-section-mode-dired-plus nil)

  ;; GNU GLOBAL: Tag management mode
  (defvar tqnr-section-mode-gnu-global nil)
  (progn ;; tqnr-section-mode-gnu-global
    ;; ggtags interface
    (defvar tqnr-section-mode-gnu-global-ggtags nil)
    ) ;; (progn ;; tqnr-section-mode-gnu-global

  ;; EPROJECT: (grischka): project management mode (never used)
  (defvar tqnr-section-mode-eproject nil)

  ;; RTRT SCRIPT: rtrt script mode (syntax coloration)
  (defvar tqnr-section-mode-rtrt-script nil)

  ;; VC CLEARCASE: vc ClearCase mode (not used)
  (defvar tqnr-section-mode-vc-clearcase nil)

  ;; CLEARCASE: ClearCase mode
  (defvar tqnr-section-mode-clearcase nil)

  ;; AUTOHOTKEY: AutoHotKey mode
  (defvar tqnr-section-mode-autohotkey nil)

  ;; OUTLINE: to manually hide some block in code source
  (defvar tqnr-section-mode-outline nil)
  (progn ;; tqnr-section-mode-outline
    ;; HIDE ALL AT START: hide all when opening file
    (defvar tqnr-section-mode-outline-hide-all-at-start nil)
    ) ;; (progn ;; tqnr-section-mode-outline

  ;; AUTO HIGHLIGHT SYMBOL: to automatically highlight symbol at point
  (defvar tqnr-section-mode-auto-highlight-symbol nil)

  ;; GOOGLE CALENDAR: to import Google calendar
  (defvar tqnr-section-mode-google-calendar nil)

  ;; FILL COLUMN INDICATOR: show a vertical line at fill-column column or customize it
  (defvar tqnr-section-mode-fill-column-indicator nil)
  (progn ;; tqnr-section-mode-fill-column-indicator
    ;; pixel width of vertical line default 1 (nil)
    (defvar tqnr-profile-fill-column-indicator-vertical-line-width nil)
    ;; color of vertical line in color format or nil (set comment theme face)
    (defvar tqnr-profile-fill-column-indicator-vertical-line-color nil)
    ;; Use a fixed column for vertical line to not use fill-column value otherwise nil
    (defvar tqnr-profile-fill-column-indicator-vertical-line-position nil)
    ;; enable vertical line in C mode
    (defvar tqnr-profile-fill-column-indicator-mode-c nil)
    ;; enable vertical line in C++ mode
    (defvar tqnr-profile-fill-column-indicator-mode-c++ nil)
    ;; enable vertical line in ADA mode
    (defvar tqnr-profile-fill-column-indicator-mode-ada nil)
    ;; enable vertical line in all mode
    (defvar tqnr-profile-fill-column-indicator-mode-all nil)
    ) ;; (progn ;; tqnr-section-mode-fill-column-indicator

  ;; MUSE: muse mode to have nice doc
  (defvar tqnr-section-mode-muse nil)

  ;; UNDO TREE: replace the undo built in function
  (defvar tqnr-section-mode-undo-tree nil)

  ;; CSV: parse/edit/sort CSV file
  (defvar tqnr-section-mode-csv nil)

  ;; SUBVERSION: support Subversion 1.7
  (defvar tqnr-section-mode-subversion nil)

  ;; DIFF COLOR: add color to diff mode
  (defvar tqnr-section-mode-diff-color nil)

  ;; DIRED SORT: more option to sort in Dired mode
  (defvar tqnr-section-mode-dired-sort nil)

  ;; ORG MODE: to organize everything (also use on Android)
  (defvar tqnr-section-mode-org-mode nil)
  (progn ;; tqnr-section-mode-org-mode
    ;; set org directory where every org file will goes
    (defvar tqnr-profile-org-directory "")
    ;; default org file where all task/todo capture will goes
    (defvar tqnr-profile-org-default-notes-file "")
    ;; agenda will look only in default org file
    (defvar tqnr-profile-org-agenda-files "")
    ;; first buffer to show up is default org file when opening emacs
    (defvar tqnr-section-mode-org-default-as-init-buffer nil)
    ) ;; (progn ;; tqnr-section-mode-org-mode

  ;; ISEARCH PLUS: add some features to isearch
  (defvar tqnr-section-mode-isearch-plus nil)

  ;; PSVN: add an icon in modeline where color give status of SVN
  (defvar tqnr-section-mode-psvn nil)

  ;; POWERLINE: fancy modeline
  (defvar tqnr-section-mode-powerline nil)

  ;; NYAN: add bar in modeline given position in buffer
  (defvar tqnr-section-mode-nyan nil)

  ;; SML: show position in modeline as a scrollbar
  (defvar tqnr-section-mode-sml nil)

  ;; DIRED: change option to command ls for dired mode
  (defvar tqnr-section-mode-dired nil)

  ;; ISEARCH: scroll is possible when incremental search
  (defvar tqnr-section-mode-isearch nil)

  ;; RAINBOW DELIMITERS: highlight nested parentheses, brackets in different color depending of depth
  (defvar tqnr-section-mode-rainbow-delimiters nil)

  ;; DIRED DETAILS: show hide details in dired mode
  (defvar tqnr-section-mode-dired-details nil)

  ;; SMART TAB: expand or indent at the point with tab
  (defvar tqnr-section-mode-smart-tab nil)

  ;; FOLD DWIM: show hide code source block
  (defvar tqnr-section-mode-fold-dwim nil)

  ;; DIRED LIS: activate by default isearch in dired mode
  (defvar tqnr-section-mode-dired-lis nil)

  ;; NXHTML: nXhtml: enhance html mode
  (defvar tqnr-section-mode-nxhtml nil)

  ;; FASTNAV: fast navigation like with zap-to-char but only to move
  (defvar tqnr-section-mode-fastnav nil)

  ;; MRU YANK: (Most Recently Used) in kill-ring
  (defvar tqnr-section-mode-mru-yank nil)

  ;; ACK: search with ack (no more grep) (need perl interpreter)
  (defvar tqnr-section-mode-ack nil)

  ;; ACE JUMP
  ;; move quickly and easily with ace jump
  ;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
  (defvar tqnr-section-mode-ace-jump nil)

  ;; AVY: move quickly and easily with avy (replacement of ace jump)
  (defvar tqnr-section-mode-avy nil)

  ;; DIREDFUL: color dired buffer
  (defvar tqnr-section-mode-diredful nil)

  ;; PS2PDF
  ;; print buffer/region in pdf (the pdf background is unavoidably white so dark
  ;; theme don't render good)
  (defvar tqnr-section-mode-ps2pdf nil)

  ;; AUCTEX: latex mode
  (defvar tqnr-section-mode-auctex nil)

  ;; YASCROLL
  ;; add a small visual scroll-bar (can not be used with mouse click)
  ;; see https://github.com/m2ym/yascroll-el for screenshot
  (defvar tqnr-section-mode-yascroll nil)
  (progn ;; tqnr-section-mode-yascroll
    ;; time before hide scroll-bar (nil to always show)
    (defvar tqnr-profile-yascroll-delay-to-hide nil)
    ) ;; (progn ;; tqnr-section-mode-yascroll

  ;; SMART FORWARD: move in code with semantic see example in plugins/smart-forward.el
  (defvar tqnr-section-mode-smart-forward nil)

  ;; RAINBOW: show string color in color
  (defvar tqnr-section-mode-rainbow nil)

  ;; EDIFF: graphical diff (## to toggle whitespace ignoring)
  (defvar tqnr-section-mode-ediff nil)

  ;; MAGIT: use git with nice interface (do not use vc interface from emacs)
  (defvar tqnr-section-mode-magit nil)

  ;; SYNERGY: use synergy without java client GUI (do not use vc interface from emacs)
  (defvar tqnr-section-mode-synergy nil)
  (progn ;; tqnr-section-mode-synergy
    ;; login to connect to synergy server
    (defvar tqnr-profile-synergy-username "")
    ;; database path to connect to synergy server
    (defvar tqnr-profile-synergy-database "")
    ;; server url to connect to synergy server
    (defvar tqnr-profile-synergy-server "")
    ;; command line to modify history output
    ;; by example: '("|" "sed" "s/login/readable_name/")
    (defvar tqnr-profile-synergy-history-filter '())
    ;; external tool to do diff with synergy
    (defvar tqnr-profile-synergy-diff-external-command "")
    ;; command line parameter to external tool to do diff
    (defvar tqnr-profile-synergy-diff-external-parameter "")
    ;; swap files in diff (left/right) about external diff tool
    (defvar tqnr-profile-synergy-diff-external-swap-file "")
    ) ;; (progn ;; tqnr-section-mode-synergy

  ;; HIDE LINES: hide lines using regexp (like narrow but with regex and not region)
  (defvar tqnr-section-mode-hide-lines nil)

  ;; AGGRESSIVE INDENT: indent all line in function/condition in C or lisp mode when edit it
  (defvar tqnr-section-mode-aggressive-indent nil)

  ;; PLATINUM SEARCHER: A front-end for pt, The Platinum Searcher (faster than ack)
  (defvar tqnr-section-mode-platinum-searcher nil)
  (progn ;; tqnr-section-mode-platinum-searcher
    ;; path to pt executable
    (defvar tqnr-profile-mode-platinum-searcher-exec "pt.exe")
    ) ;; (progn ;; tqnr-section-mode-platinum-searcher

  ;; POPWIN: A pop-up manager for annoying buffer (have like ECB compilation buffer)
  (defvar tqnr-section-mode-popwin nil)

  ;; PROJECTILE: Project management, filtered find-file, only with root file from version control
  (defvar tqnr-section-mode-projectile nil)

  ;; COMPANY: Completion mode using external back-ends to have symbol
  (defvar tqnr-section-mode-company nil)

  ;; EXPAND REGION: Increase selected region by semantic units
  (defvar tqnr-section-mode-expand-region nil)

  ;; FUNCTION ARGS: Show function parameters in C and C++
  (defvar tqnr-section-mode-function-args nil)

  ;; ELPY
  ;; Python mode like an IDE (only install is from package)
  ;;  ;; add elpy package
  ;;  ;; and flycheck package, about warnings/errors check on the fly
  ;;  ;; and autopep8 package, about fix automagically some pep8 rules after save python file
  ;;  (add-to-list 'profile-environment-elpa-package-list 'elpy t)
  ;;  (add-to-list 'profile-environment-elpa-package-list 'flycheck t)
  ;;  (add-to-list 'profile-environment-elpa-package-list 'py-autopep8 t)
  (defvar tqnr-section-mode-elpy nil)
  (progn ;; tqnr-section-mode-elpy
    ;; override path for created elpy virtualenv (should have rights to execute)
    (defvar tqnr-section-mode-elpy-rpc-virtualenv-path nil)
    ) ;; (progn ;; tqnr-section-mode-elpy

  ;; SMARTPARENS: useful to have nice navigation through source code structure
  (defvar tqnr-section-mode-smartparens nil)

  ;; PLANTUML: generate uml diagram from text
  (defvar tqnr-section-mode-plantuml nil)

  ;; GRAPHVIZ DOT: generate diagram from text
  (defvar tqnr-section-mode-graphviz-dot nil)

  ;; HASKELL: editing, debugging and developing Haskell programs
  (defvar tqnr-section-mode-haskell nil)

  ;; CFLOW: useful to have call tree in C source code
  (defvar tqnr-section-mode-cflow nil)

  ;; IRONY: improving the editing experience for the C, C++ and Objective-C using clang
  (defvar tqnr-section-mode-irony nil)

  ;; MARKDOWN: mode to edit Markdown-formatted text (by example wiki of github)
  (defvar tqnr-section-mode-markdown nil)
  (progn ;; tqnr-section-mode-markdown
    ;; to enable markdown mode with github flavoured for all .md files and not
    ;; only for README.md
    (defvar tqnr-section-mode-markdown-github nil)
    ) ;; (progn ;; tqnr-section-mode-markdown

  ;; EASY KILL: mode to easy copy/kill/cut text/line/word/expression/function...
  (defvar tqnr-section-mode-easy-kill nil)

  ;; ARDUINO: mode to enable c mode for .ino files and use emacs as external editor of arduino ide
  (defvar tqnr-section-mode-arduino nil)

  ;; ALL THE ICONS
  ;; mode to have nice icons (from special fonts)
  ;; install font on your system from `fonts' folder or use
  ;; M-x all-the-icons-install-fonts
  (defvar tqnr-section-mode-all-the-icons nil)

  ;; SHACKLE
  ;; mode to have popup always following same rules
  ;; like popwin but just add constraint to popup not replace the whole thing
  ;; Helm does not like popwin...
  (defvar tqnr-section-mode-shackle nil)

  ;; RIPGREP
  ;; A front-end for rg, ripgrep (faster than anything...)
  ;; use .ripgreprc to add new type
  (defvar tqnr-section-mode-ripgrep nil)

  ;; HYDRA: Create families of short bindings with a common prefix
  (defvar tqnr-section-mode-hydra nil)
  (progn ;; tqnr-section-mode-hydra
    ;; Use Hydra to manage rectangle shortcuts
    (defvar tqnr-section-mode-hydra-rectangle nil)
    ;; Use Hydra to manage windows/frame/buffer shortcuts
    (defvar tqnr-section-mode-hydra-display nil)
    ;; Use Hydra to manage transpose shortcuts
    (defvar tqnr-section-mode-hydra-transpose nil)
    ;; Use Hydra to manage help/web shortcuts
    (defvar tqnr-section-mode-hydra-help-web nil)
    ;; Use Hydra to manage macro shortcuts
    (defvar tqnr-section-mode-hydra-macro nil)
    ;; Use Hydra to manage spelling shortcuts
    (defvar tqnr-section-mode-hydra-spelling nil)
    ;; Use Hydra to manage search shortcuts
    (defvar tqnr-section-mode-hydra-search nil)
    ;; Use Hydra to manage smartparens shortcuts
    (defvar tqnr-section-mode-hydra-smartparens nil)
    ;; Use Hydra to manage ada compile shortcuts
    (defvar tqnr-section-mode-hydra-ada nil)
    ;; Use Hydra to manage outline shortcuts
    (defvar tqnr-section-mode-hydra-outline nil)
    ;; Use Hydra to manage org shortcuts
    (defvar tqnr-section-mode-hydra-org-mode nil)
    ;; Use Hydra to manage special buffer toggle shortcuts
    (defvar tqnr-section-mode-hydra-special-buffer nil)
    ) ;; (progn ;; tqnr-section-mode-hydra

  ;; FLYSPELL: On-the-fly spell checking
  (defvar tqnr-section-mode-flyspell nil)
  (progn ;; tqnr-section-mode-flyspell
    ;; set program to be use with ispell
    (defvar tqnr-profile-ispell-program "aspell")
    ;; language to use with ispell
    (defvar tqnr-profile-ispell-dictionary "english")
    ;; POPUP: Correct the misspelled word in popup menu
    (defvar tqnr-section-mode-flyspell-popup nil)
    ) ;; (progn ;; tqnr-section-mode-flyspell

  ;; DUMB JUMP: On-the-fly spell checking
  (defvar tqnr-section-mode-dumb-jump nil)

  ;; ADA: Ada mode for edit/navigate/compile ada source code
  (defvar tqnr-section-mode-ada nil)

  ;; FITNESSE: FitNesse MarkUp files syntax highlight
  (defvar tqnr-section-mode-fitnesse nil)

  ;; PANDOC: PanDoc tools mode to translate between markup syntax
  (defvar tqnr-section-mode-pandoc nil)

  ;; FLEX ISEARCH: Flex Isearch mode add fuzzy match when doing incremental search
  (defvar tqnr-section-mode-flex-isearch nil)

  ;; ORG JIRA: Sync JIRA issues with org-mode issues
  (defvar tqnr-section-mode-org-jira nil)

  ;; GNUPLOT: Major mode for editing gnuplot scripts
  (defvar tqnr-section-mode-gnuplot nil)

  ;; POWERSHELL: Powershell mode
  (defvar tqnr-section-mode-powershell nil)

  ;; HELPFUL: Helpful mode
  (defvar tqnr-section-mode-helpful nil)

  ;; IALIGN: ialign package
  (defvar tqnr-section-mode-ialign nil)

  ;; REALGUD: realgud mode
  (defvar tqnr-section-mode-realgud nil)

  ;; MOVE TEXT: realgud mode
  (defvar tqnr-section-mode-move-text nil)

  ;; GROOVY: groovy mode
  (defvar tqnr-section-mode-groovy nil)

  ;; UNDO FU: replace the undo built in function without same problem than undo-tree
  (defvar tqnr-section-mode-undo-fu nil)

  ;; UNDO FU SESSION: Save & recover undo steps between Emacs sessions
  (defvar tqnr-section-mode-undo-fu-session nil)
  (progn ;; tqnr-section-mode-undo-fu-session
    ;; set undo fu session directory where every undo will be stored
    (setq tqnr-profile-undo-fu-session-directory (concat (file-name-as-directory tqnr-dotemacs-path) "undo"))
    ;; set file size limit about session save file
    (setq tqnr-profile-undo-fu-session-file-limit 1024)
    ) ;; (progn ;; tqnr-section-mode-undo-fu-session

  ;; HEADER2: Create/Update header of files
  (defvar tqnr-section-mode-header2 nil)

  ;; LSP MODE: Lsp mode
  (defvar tqnr-section-mode-lsp-mode nil)

  ;; LUA MODE: Lua mode
  (defvar tqnr-section-mode-lua-mode nil)

  ;; PROJECTILE DIRENV: Projectile direnv mode to set environment variable as found in direnv config
  (defvar tqnr-section-mode-projectile-direnv nil)

  ;; DIMINISH: shrink major and minor mode name in the modeline
  (defvar tqnr-section-mode-diminish nil)
  ) ;; (progn ;; tqnr-section-mode

;; LANGUAGES: Set style and/or indentation for multiple languages
(defvar tqnr-section-languages nil)
(progn ;; tqnr-section-languages
  ;;
  ;; C
  ;;   language settings, set indentation style and preprocessing option
  (defvar tqnr-section-languages-c nil)
  (progn ;; tqnr-section-languages-c
    ;; number of space for indentation in C
    (defvar tqnr-profile-c-indent-offset 2)
    ;; new types (add name string in list)
    (defvar tqnr-profile-c-extra-types '())
    ;; Compile mode without ask
    (defvar tqnr-profile-c-ask-before-compile t)
    ;; INDENT PREPROCESSOR: make a #define be align with C code
    (defvar tqnr-section-languages-c-indent-preprocessor nil)
    ;; FLYMAKE: verification error/warning in source code on the fly
    (defvar tqnr-section-languages-c-flymake nil)
    ;; FLYCHECK: verification error/warning in source code on the fly
    (defvar tqnr-section-languages-c-flycheck nil)
    ;; command to preprocess
    (defvar tqnr-profile-c-macro-preprocessor "")
    ;; set flags about macro preprocessing
    (defvar tqnr-profile-c-macro-cppflags "")
    ) ;; (progn ;; tqnr-section-languages-c
  ;;
  ;; LISP
  ;;   set indentation style
  (defvar tqnr-section-languages-lisp nil)
  (progn ;; tqnr-section-languages-lisp
    ;; number of space for indentation in lisp
    (defvar tqnr-profile-lisp-indent-offset 2)
    ) ;; (progn ;; tqnr-section-languages-lisp
  ;;
  ;; TABULATION
  ;;   tab always in space
  (defvar tqnr-section-languages-tabulation nil)
  ;;
  ;; RTRT SCRIPT PTU
  ;;   set indentation style
  (defvar tqnr-section-languages-rtrt-script nil)
  (progn ;; tqnr-section-languages-rtrt-script
    ;; set number of space for indentation in rtrt script .ptu
    (defvar tqnr-profile-rtrt-indent-offset 4)
    ) ;; (progn ;; tqnr-section-languages-rtrt-script
  ;;
  ;; PERL
  ;;   set indentation style
  (defvar tqnr-section-languages-perl nil)
  (progn ;; tqnr-section-languages-perl
    ;; number of space for indentation in perl
    (defvar tqnr-profile-perl-indent-offset 2)
    ) ;; (progn ;; tqnr-section-languages-perl
  ;;
  ;; C++ QT
  ;;   set include for Qt 4.8
  (defvar tqnr-section-languages-c++-qt nil)
  ;;
  ;; ARDUINO
  ;;   set indentation style
  (defvar tqnr-section-languages-arduino nil)
  (progn ;; tqnr-section-languages-arduino
    ;; number of space for indentation in Arduino
    (defvar tqnr-profile-arduino-indent-offset 2)
    ) ;; (progn ;; tqnr-section-languages-arduino
  ) ;; (progn ;; tqnr-section-languages

;; SELECTION: selection can be kill + selection is highlight + kill->copy in read only
(defvar tqnr-section-selection nil)
(progn ;; tqnr-section-selection
  ;; SHIFT SELECTION
  (defvar tqnr-section-selection-with-shift nil)
  ) ;; (progn ;; tqnr-section-selection

;; DISPLAY: modification about display in buffers, font, color...
(defvar tqnr-section-display nil)
(progn ;; tqnr-section-display

  ;; BUFFER
  ;; buffers with *buffername* should be displayed in the same window
  ;; first column in window will display buffer limit, next page will leave 5 shared line
  (defvar tqnr-section-display-buffer nil)
  (progn ;; tqnr-section-display-buffer
    ;; VISUAL LINE: word wrap, truncate line without cut word
    ;; END and HOME will go to the end/start of screen line not logical line
    (defvar tqnr-section-display-windows-buffers-visual-line nil)
    ) ;; (progn ;; tqnr-section-display-buffer

  ;; SPEEDBAR: set size and display of speedbar (see GLOSSARY) (no used)
  (defvar tqnr-section-display-speedbar nil)

  ;; FONT: set font in terminal or in graphical
  (defvar tqnr-section-display-font nil)
  (progn ;; tqnr-section-display-font
    ;; Font family and size:
    ;; choice between (it's just some nice font, you can use another font):
    ;;
    ;; "Terminal-6"
    ;; nice, very tiny, only ascii (too tiny ?)
    ;;
    ;; "Anonymous Pro-10"
    ;; nice, big (slashed 'zero', 'one' and minus 'L' can be mixed up)
    ;; "Anonymous Pro-8"
    ;; nice, small (slashed 'zero', 'one' and minus 'L' can be mixed up, parentheses and curly bracket can be mixed up)
    ;;
    ;; "ProggyTinySZ-6"
    ;; good, very tiny (slashed 'zero', dot and comma can be mixed)
    ;;
    ;; "DejaVu Sans Mono-10"
    ;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
    ;; "DejaVu Sans Mono-8"
    ;; not so nice with ms window (dot 'zero', capitalized 'i' and minus 'L' can be mixed up)
    ;;
    ;; "Inconsolata-10"
    ;; not so good with ms window (slashed 'zero', capitalized 'i' and minus 'L' can be mixed up)
    ;;
    ;; "Lucida Console-10"
    ;; nice, big, large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
    ;; "Lucida Console-8"
    ;; nice, small large (not slashed 'zero' so 'zero' and capitalized 'o' can be mixed up)
    ;;
    ;; "Monaco-10"
    ;; nice, very big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
    ;; "Monaco-8"
    ;; nice, big, large (slashed 'zero', 'dot' and 'comma' can be mixed up)
    ;;
    ;; "ProFontWindows-8"
    ;; nice, tiny, (slashed 'zero', 'one' and minus 'L' can be mixed up)
    ;;
    ;; "Courier New-10"
    ;; classic but big and large
    ;; "Courier New-8"
    ;; classic but big and large
    ;;
    ;; "Ubuntu Mono-10"
    ;; "Ubuntu Mono-8"
    ;;
    ;; "Terminus-10"
    ;; "Terminus-8"
    ;;
    ;; "Monospace-8"
    ;;
    ;; "Liberation Mono-8"
    ;;
    ;; "Inconsolata-10"
    ;; "Inconsolata-8"
    ;;
    ;; "Droid Sans Mono-10"
    ;; "Droid Sans Mono-8"
    ;;
    ;; "Iosevka-10"
    ;; "Iosevka-8"
    ;; nice can use any variant of this open source font (by example slashed zero, dot zero, empty zero)
    ;; see https://be5invis.github.io/Iosevka/ for more settings
    ;;
    (defvar tqnr-profile-font "Lucida Console-10")
    ;; ANTIALIAS: set antialiasing on font rendering
    (defvar tqnr-section-display-font-antialias nil)
    ) ;; (progn ;; tqnr-section-display-font

  ;; COLOR: set color in emacs
  (defvar tqnr-section-display-color nil)
  (progn ;; tqnr-section-display-color
    ;; PARENTHESES MODE: matched parentheses are highlight
    (defvar tqnr-section-display-color-parentheses-mode nil)
    ;; PARENTHESES MINIBUFFER: matched parentheses are highlight and if not
    ;; visible show it in the Minibuffer
    (defvar tqnr-section-display-color-parentheses-visible nil)
    ;; PARENTHESES HIGHLIGHT: matched parentheses are highlight in rainbow color
    (defvar tqnr-section-display-color-parentheses-highlight nil)
    ;; COLOR THEME: set color by color-theme mode (or manual settings nil)
    (defvar tqnr-section-display-color-theme nil)
    ;; theme to be used, do not use it with terminal
    (defvar tqnr-profile-color-theme nil)
    ;; ANSI COLOR COMPILE WINDOW: have color and no more junk like this ^[[32m
    (defvar tqnr-section-display-color-ansi-color-compile nil)
    ;; HIGHLIGHT CURRENT LINE: have current line highlighted
    (defvar tqnr-section-display-color-highlight-line nil)
    ) ;; (progn ;; tqnr-section-display-color
  ) ;; (progn ;; tqnr-section-display

;; INTERFACE: modification of GNU Emacs interface, size, title, decoration...
(defvar tqnr-section-interface nil)
(progn ;; tqnr-section-interface

  ;; MAIN WINDOW: modification about main window of emacs
  (defvar tqnr-section-interface-main-window nil)
  ;;
  (progn ;; tqnr-section-interface-main-window
    ;; DECORATION: remove all mouse interface (toolbar, menubar, scrollbar)
    (defvar tqnr-section-interface-remove-decoration nil)
    ;;
    ;; WINDOW TITLE: buffer name in title bar (example "< foobar.c >") (from grandm_y)
    ;; %b buffername ; %F frame name ; %l line number ; %c column number
    ;; %p percent of buffer above top ; %m mode name ; %n Narrow mode
    ;; %z coding systems ; %Z %z + end-of-line format ; %- infinitely dashes
    (defvar tqnr-profile-window-title "GNU Emacs - %b")
    ;;
    ;; TRANSPARENCY: the whole emacs window will be transparent
    (defvar tqnr-section-interface-transparency nil)
    ;; transparency of the window. 0=transparent/100=opaque
    (defvar tqnr-profile-transparency 96)
    ;;
    ;; FULLSCREEN: main window start in fullscreen
    (defvar tqnr-section-interface-fullscreen nil)
    ;;
    ;; Do not popup any window by splitting vertically only horizontally
    (defvar tqnr-section-interface-popup-window-horizontally nil)
    ) ;; (progn ;; tqnr-section-interface-main-window

  ;; MODELINE
  ;; set some option to add in the grey line at the bottom of each buffer
  ;; (replaced by powerline mode)
  (defvar tqnr-section-interface-modeline nil)

  ;; ECB: set size, display, refresh and remove opening tips of ECB window
  (defvar tqnr-section-interface-ecb nil)
  (progn ;; tqnr-section-interface-ecb
    ;; ECB ICON FOR TREE: display icon image instead of ascii guides for arborescence tree
    (defvar tqnr-section-interface-ecb-ascii-tree nil)
    ) ;; (progn ;; tqnr-section-interface-ecb
  ) ;; (progn ;; tqnr-section-interface

;; COMPLETION: enable letter case completion + dynamic completion
(defvar tqnr-section-completion nil)

;; SHORTCUT: custom binding or shortcut for everything in GNU Emacs (thought for qwerty keyboard)
(defvar tqnr-section-shortcut nil)
(progn ;; tqnr-section-shortcut

  ;; GLOBAL: add global shortcut (for whole Emacs)
  (defvar tqnr-section-shortcut-global nil)
  (progn ;; tqnr-section-shortcut-global
    ;; CUA: enable C-x, C-c, C-v to cut copy paste
    ;; don't recommend it otherwise see http://www.emacswiki.org/CuaMode
    (defvar tqnr-section-shortcut-global-cua nil)
    ) ;; (progn ;; tqnr-section-shortcut-global

  ;; WINDOWS: add shortcut to manage windows
  (defvar tqnr-section-shortcut-windows nil)

  ;; BUFFER: add shortcut to manage buffers
  (defvar tqnr-section-shortcut-buffer nil)

  ;; GREP: add shortcut to manage grep
  (defvar tqnr-section-shortcut-grep nil)

  ;; ETAG: add shortcut to manage gtags or etags
  (defvar tqnr-section-shortcut-etag nil)

  ;; SEMANTIC: add shortcut to move in source code with semantic
  (defvar tqnr-section-shortcut-semantic nil)

  ;; HOOK: run hook for all shortcuts from the whole configuration
  (defvar tqnr-section-shortcut-hook nil)
  ) ;; (progn ;; tqnr-section-shortcut

;; MOUSE: smooth wheel + lazy decoration when scroll
(defvar tqnr-section-mouse nil)
(progn ;; tqnr-section-mouse
  ;; PASTE CURSOR: yank at point and not at mouse cursor (either when yank with mouse wheel)
  (defvar tqnr-section-mouse-paste-to-point-not-mouse-cursor nil)
  ;; AVOIDANCE: move mouse cursor at top right of the buffer to not bother me
  (defvar tqnr-section-mouse-avoidance nil)
  ;; SMOOTH SCROLL: scroll with margin and without jump
  (defvar tqnr-section-mouse-smooth-scroll nil)
  ) ;; (progn ;; tqnr-section-mouse

;; ANNOYANCES
;; no welcome message + yes->y + do not query to refresh buffer + remove insert
;; key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog box + no
;; tooltips
(defvar tqnr-section-annoyances nil)
(progn ;; tqnr-section-annoyances
  ;; ask confirmation to quit Emacs
  (defvar tqnr-section-annoyances-comfirm-quit nil)
  ;;
  ;; TRUNCATE LINE: whole line not visible (need to scroll right)
  (defvar tqnr-section-annoyances-truncate-line nil)
  ;;
  ;; SCROLL PRESERVE CURSOR POSITION: when wheel scroll the cursor do not move
  (defvar tqnr-section-annoyances-scroll-preserve-cursor-position nil)
  ;;
  ;; NO BACKUP FILE: turn off backup files
  (defvar tqnr-section-annoyances-no-backup-file nil)
  ;;
  ;; ALL BACKUP FILE IN DIRECTORY: all backup files goes in a directory
  (defvar tqnr-section-annoyances-backup-file-in-directory nil)
  (progn ;; tqnr-section-annoyances-backup-file-in-directory
    ;; all backup files goes in a directory
    (defvar tqnr-profile-backup-directory nil)
    (defvar tqnr-profile-autosave-directory nil)
    ) ;; (progn ;; tqnr-section-annoyances-backup-file-in-directory
  ) ;; (progn ;; tqnr-section-annoyances

;; MISC
;; remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
(defvar tqnr-section-misc nil)
(progn ;; tqnr-section-misc
  ;; !!!PRIVATE!!! all theses variable should be put in private file of profile
  ;; USERNAME: define user name
  (defvar tqnr-profile-username "")
  (defvar tqnr-profile-login "")
  ;; !!!PRIVATE!!! End
  ;;
  ;; SPACE: remove useless space at the end of line
  (defvar tqnr-profile-remove-useless-ending-space nil)
  ;; END OF FILE: be sure that a new line is at the end of a file when it's saved
  (defvar tqnr-profile-always-new-line-at-end nil)
  ;; COLUMN: fill-xxx is set with a width
  (defvar tqnr-profile-fill-column 80)
  ;;
  ;; WEB: set browser to open url
  (defvar tqnr-profile-browser "firefox")
  ;;
  ;; CALENDAR: set latitude/longitude + location + holidays + custom date in
  ;; Modeline lunar phase, sunrise/sunset, time, etc
  (defvar tqnr-section-misc-calendar nil)
  (progn ;; tqnr-section-misc-calendar
    ;; !!!PRIVATE!!! all theses variable should be put in private file of profile
    ;; LOCALIZATION: set latitude/longitude + location + holidays + custom date in
    ;; Modeline lunar phase, sunrise/sunset, time etc
    (defvar tqnr-profile-latitude 0.00)
    (defvar tqnr-profile-longitude 0.00)
    (defvar tqnr-profile-location-name "Neverland")
    ;; !!!PRIVATE!!! End
    ;; FRENCH CALENDAR: set French holidays and day/month/moon phase name
    (defvar tqnr-section-misc-calendar-french nil)
    ) ;; (progn ;; tqnr-section-misc-calendar
  ;; BOOKMARK: default file, each command to add/modify bookmark save bookmark file
  (defvar tqnr-section-misc-bookmark nil)
  (progn ;; tqnr-section-misc-bookmark
    ;; BOOKMARK SORT: sort or do not sort bookmark when saving bookmark file
    (defvar tqnr-profile-bookmark-sort nil)
    ) ;; (progn ;; tqnr-section-misc-bookmark
  ;; SCREENSAVER: when idle for 5min some animations on buffer text
  (defvar tqnr-section-misc-screensaver nil)
  ) ;; (progn ;; tqnr-section-misc

;; SAFE LOCAL VARIABLE: all customize settings from any .dir-local.el are put in customize
(defvar tqnr-section-safe-local-variable nil)

;; FILECUSTOMIZE
;; all customize settings are put in here when you use interface
;; (customize) to change settings
(defvar tqnr-section-filecustomize nil)


(provide 'init-profile)

;;; init-profile.el ends here
