;;; profile-thales-avionics-server.el --- a config file for profile

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

;;; Code:
;; load private variable
(try-require 'profile-thales-avionics-server-private "    ")

;; Enable/disable section:
;; (setq tqnr-section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' (disable) or 't' (enable) value
;;
;; Settings:
;; (setq tqnr-profile-xxx X)

;; ENVIRONMENT: Environment check and configuration
(setq tqnr-section-environment t)
(when tqnr-section-environment
  ;; GARBAGE COLLECTION: speed up start of emacs
  (setq tqnr-section-environment-garbage-collection t)
  (when tqnr-section-environment-garbage-collection
    ;; MINIBUFFER: increase temporarily garbage collection when execute something
    (setq tqnr-section-environment-garbage-collection-minibuffer t)
    ;; START: speed up a little the start of emacs
    (setq tqnr-environment-garbage-collection-start nil)
    ) ;; (when tqnr-section-environment-garbage-collection
  ;; VERSION RECOGNITION: detect Emacs version
  (setq tqnr-section-environment-version-recognition t)
  ;; OS RECOGNITION: detect OS type (MS Windows vs GNU Linux)
  (setq tqnr-section-environment-os-recognition t)
  ;; TERMINAL VS GRAPHICS: detect Emacs terminal vs graphics
  (setq tqnr-section-environment-terminal-vs-graphics t)
  ;; SET PATH: Set environment variable PATH
  (setq tqnr-section-environment-set-path t)
  (when tqnr-section-environment-set-path
    ;; PATH environment variable concat with current PATH
    (defvar tqnr-profile-portable-git-version "2.30.0")
    (setq tqnr-profile-path
      (list
        ;; before MSYS2 to make sure to use this version of tools
        ;; (do not put path which contains gcc otherwise it will be used instead of MSYS2 one)
        ;; emacs useful binary
        ;;(concat (file-name-as-directory tqnr-dotemacs-path) "plugins/ada-mode")
        (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/ripgrep-x86_64")
        (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/pandoc-2.0.3")
        (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_662wb/bin")
        (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/universal-ctags-x64_2018-08-22")
        (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/cflow-mingw-master")
        (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/irony-mode/server/build/bin")
        ;; gnuplot
        "k:/GNUPLOT/gnuplot_5.3/bin"
        ;; GNATPRO
        "k:/GNATPRO/GNATPRO_18.2/bin"
        ;; PYTHON
        "j:/PYTHON/PYTHON_3.6.6"
        "j:/PYTHON/PYTHON_3.6.6/Scripts"
        ;; GIT
        (concat  "k:/GIT/GIT_" tqnr-profile-portable-git-version "/bin")
        ;; MSYS (i686)
        "j:/MSYS/msys64/mingw64/bin"
        "j:/MSYS/msys64/usr/local/bin"
        "j:/MSYS/msys64/usr/bin"
        "j:/MSYS/msys64/bin"
        ;; GITEXTENSIONS
        "j:/GIT_EXTENSIONS/GitExtensions_4.0.0.10352-ad275d6d4"
        ;; CYGWIN
        ;;"D:/App/cygwin/bin"
        ;; GIT linux utils (msys64 override it)
        (concat "k:/GIT/GIT_" tqnr-profile-portable-git-version "/cmd")
        (concat "k:/GIT/GIT_" tqnr-profile-portable-git-version "/mingw64/bin")
        (concat "k:/GIT/GIT_" tqnr-profile-portable-git-version "/usr/bin")
        ;; GPS
        "k:/GPS/GPS_20.0_x64/bin"
        ;; GNU Emacs
        "j:/EMACS/emacs-26.3-x86_64/bin"
        ;; MS Windows usual path
        "C:/WINDOWS"
        "C:/WINDOWS/System32"
        )
      ) ;; (setq tqnr-profile-path
    ;;
    ;; LOCALE: languages settings about subversion and dired
    (setq tqnr-profile-lang "en_US")
    ;; universal-ctags as backend of GNU Global
    (setenv "GTAGSCONF" (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_662wb/share/gtags/gtags.conf"))
    (setenv "GTAGSLABEL" "new-ctags")
    (setenv "RIPGREP_CONFIG_PATH" "u:/.ripgreprc")
    ) ;; (when tqnr-section-environment-set-path
  ;; MS WINDOWS PERFORMANCE: MS Windows specific configuration about performance
  (setq tqnr-section-environment-ms-windows-performance t)
  ;; EXECUTABLE: Set path of some exe
  (setq tqnr-section-environment-executable t)
  (when tqnr-section-environment-executable
    ;; shell program
    (setq tqnr-profile-shell-program "bash")
    ;; shell program arguments
    (setq tqnr-profile-shell-arguments "-lc")
    ;; diff program
    (setq tqnr-profile-ediff-diff-program "diff")
    (setq tqnr-profile-ediff-diff3-program "diff3")
    (setq tqnr-profile-ediff-cmp-program "cmp")
    ) ;; (when tqnr-section-environment-executable
  ;; HYPER: Enable Hyper modifier key (Menu key, shortcut with "H-") on MS Windows
  (setq tqnr-section-environment-hyper t)
  ;; SUPER: Enable Super modifier key (Windows key, shortcut with "s-") on MS Windows
  (setq tqnr-section-environment-super t)
  ;; SERVER: start the emacs server to have only one emacs client instance
  (setq tqnr-section-environment-server t)

  ;; PACKAGE: package management
  (setq tqnr-section-environment-package t)
  (when tqnr-section-environment-package
    ;; PROXY: proxy setting about package management
    (setq tqnr-profile-environment-package-proxy-http nil)
    (setq tqnr-profile-environment-package-proxy-https nil)
    ;; LOCAL PATH: proxy setting about package management
    (setq tqnr-profile-environment-package-local-path "j:/EMACS/emacs-package-archives")
    ;; PACKAGE LIST: list of package like "'(first-package second-package)" to be installed
    (setq tqnr-profile-environment-package-list '())
    ) ;; (when tqnr-section-environment-package

  ;; SHORTCUT: Environment shortcut to declare hook about shortcut
  (setq tqnr-section-environment-shortcut t)
  ) ;; (when tqnr-section-environment

;; FUNCTION: custom functions
(setq tqnr-section-function t)
(when tqnr-section-function

  ;; GENERATE PROFILE
  ;; functions to generate profile from all config file
  ;;
  ;; interactive function `tqnr-generate-profile' will parse dotemacs directory to
  ;; generate a default profile file or with a parameter not nil init-profile.el file
  (setq tqnr-section-function-generate-profile t)

  ;; EDIT BUFFER: custom function about edition of buffer text
  (setq tqnr-section-function-edit-buffer t)
  (when tqnr-section-function-edit-buffer
    ;; FLAG DEBUG: used in kaneton project (epita)
    (setq tqnr-function-kaneton nil)
    ) ;; (when tqnr-section-function-edit-buffer

  ;; SELECT COPY: custom function about copying and selecting buffer text
  (setq tqnr-section-function-select-copy t)

  ;; SEARCH: custom function about searching in buffer text
  (setq tqnr-section-function-search t)

  ;; BUFFER WINDOW: custom function about buffer and window handling
  (setq tqnr-section-function-buffer-window t)

  ;; FILE: custom function about file management
  (setq tqnr-section-function-file t)

  ;; MACRO: custom function about macro management
  (setq tqnr-section-function-macro t)

  ;; SEMANTIC: custom function about semantic mode
  (setq tqnr-section-function-semantic t)

  ;; WEB: custom function about web interface to browser
  (setq tqnr-section-function-web t)

  ;; HELM: custom function about Helm mode
  (setq tqnr-section-function-helm t)

  ;; AUCTEX: custom function about AUCTeX
  (setq tqnr-section-function-auctex nil)

  ;; ORG MODE: custom function about org-mode
  (setq tqnr-section-function-org-mode nil)

  ;; MAGNETI MARELLI: custom functions about Magneti Marelli specific needs
  (setq tqnr-section-function-magneti-marelli nil)

  ;; ECB: custom function about ecb mode
  (setq tqnr-section-function-ecb nil)

  ;; CLEARCASE: custom function about ClearCase integration into GNU Emacs
  (setq tqnr-section-function-clearcase nil)

  ;; RTRT: custom function about RTRT script .ptu file
  (setq tqnr-section-function-rtrt nil)

  ;; ADA: functions to have multiple compile mode in ada
  (setq tqnr-section-function-ada nil)
  (when tqnr-section-function-ada
    ;; BUILD
    ;; Command of build one file in ada
    (setq tqnr-ada-gps-build-command "gnat build")
    ;; Buffer name of build one file in ada
    (setq tqnr-ada-gps-build-buffer-name "*ada-build*")
    ;; CHECK
    ;; Command of check one file in ada
    (setq tqnr-ada-gps-check-command "gnat check")
    ;; Buffer name of check one file in ada
    (setq tqnr-ada-gps-check-buffer-name "*ada-check*")
    ;; PRETTY PRINT
    ;; Command of pretty-print one file in ada
    (setq tqnr-ada-gps-pretty-print-command "gnat pretty")
    ;; Buffer name of pretty-print one file in ada
    (setq tqnr-ada-gps-pretty-print-buffer-name "*ada-pretty-print*")
    ;; BUILD ALL
    ;; Command of build all project files
    (setq tqnr-ada-gps-build-all-command "gnat make")
    ;; Buffer name of build all project files command
    (setq tqnr-ada-gps-build-all-buffer-name "*ada-build-all*")
    ;; CLEAN
    ;; Command of clean all generated file
    (setq tqnr-ada-gps-clean-command "gnat clean")
    ;; Buffer name of clean all generated command
    (setq tqnr-ada-gps-clean-buffer-name "*ada-clean*")
    ;; REBUILD ALL
    ;; Command of rebuild all generated file
    (setq tqnr-ada-gps-rebuild-all-command "gnat clean && gnat make")
    ;; Buffer name of rebuild all generated command
    (setq tqnr-ada-gps-rebuild-all-buffer-name "*ada-rebuild-all*")
    ;; BUILD NATIVE
    ;; Command of build for native execution
    (setq tqnr-ada-gps-build-native-command "gnat clean")
    ;; Buffer name of native build command
    (setq tqnr-ada-gps-build-native-buffer-name "*ada-build-native*")
    ) ;; (when tqnr-section-function-ada

  ;; CALC: functions to add type support to helm ag for ripgrep
  (setq tqnr-section-function-calc t)
  ) ;; (when tqnr-section-function

;; MODE: load extern files which are modes in plugins/
(setq tqnr-section-mode t)
(when tqnr-section-mode

  ;; USE PACKAGE: Package configuration with simple and tidy macro (mandatory)
  (setq tqnr-section-mode-use-package t)

  ;; HELM: (fork ANYTHING) choose anything with the same nice interface
  (setq tqnr-section-mode-helm t)
  (when tqnr-section-mode-helm
    ;; isearch by helm
    (setq tqnr-section-mode-helm-swoop t)
    ;; do not have default value when run helm swoop
    (setq tqnr-section-mode-helm-swoop-without-pre-input t)
    ;; compile by helm
    (setq tqnr-section-mode-helm-compile nil)
    ;; replace fuzzy search in find-files by flx, more human matches
    (setq tqnr-section-mode-helm-flx t)
    ;; replace yank-pop or browse kill ring by helm-kill-ring
    (setq tqnr-section-mode-helm-kill-ring t)
    ;; replace M-x
    (setq tqnr-section-mode-helm-M-x t)
    ;; replace electric buffer list
    (setq tqnr-section-mode-helm-buffers-list t)
    ;; add helm menu shortcut for function/variable list
    (setq tqnr-section-mode-helm-imenu t)
    ;; replace occur by helm
    (setq tqnr-section-mode-helm-occur t)
    ;; replace find files C-x C-f
    (setq tqnr-section-mode-helm-find-files t)
    ;; replace recentf
    (setq tqnr-section-mode-helm-recentf t)
    ;; replace bookmark list
    (setq tqnr-section-mode-helm-bookmark t)
    ;; enable Helm everywhere when asking file
    (setq tqnr-section-mode-helm-global t)
    ) ;; (when tqnr-section-mode-helm

  ;; HOME END: add some useful function to home and end keys
  (setq tqnr-section-mode-home-end t)

  ;; DOXYMACS: emacs interface for doxygen comments
  (setq tqnr-section-mode-doxymacs nil)

  ;; IDO
  ;; yet another switch buffer
  ;; 2010: erratic behavior with exotic filename under MS Windows (not used)
  (setq tqnr-section-mode-ido nil)

  ;; UNIQUIFY: create unique buffer names
  (setq tqnr-section-mode-uniquify t)

  ;; CEDET: "Collection of Emacs Development Environment Tools"
  (setq tqnr-section-mode-cedet t)
  (when tqnr-section-mode-cedet
    ;; bin path of gnu global for cedet
    (setq tqnr-profile-gnu-global (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_662wb/bin/global.exe"))
    (setq tqnr-profile-gnu-global-gtags (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_662wb/bin/gtags.exe"))
    ;;
    ;; CEDET SEMANTIC: can do tag, list of function/variable..., preproc, etc
    (setq tqnr-section-mode-cedet-semantic t)
    ;; list of projects: the order is important, display in reverse order (first->last)
    (setq tqnr-profile-ede-project '())
    ;;
    ;; CEDET ECB (Emacs Code Browser): transform Emacs interface to IDE
    ;; can display other windows or speedbar to view folder tree, source list,
    ;; variable/function list, buffer history, etc
    (setq tqnr-section-mode-cedet-ecb nil)
    ;; set default path in "ecb directories"
    (setq tqnr-profile-ecb-source-path '())
    ;; regexp of folder to exclude in "ecb directories"
    (setq tqnr-profile-ecb-excluded-directories-regexps '())
    ;; files to be ignored in "ecb source" !! RTFM !!
    (setq tqnr-profile-ecb-source-file-regexps '(()))
    ;; files to be ignored from Version Control VC
    (setq tqnr-profile-ecb-sources-exclude-cvsignore '())
    ;; regexp to form group in "ecb history"
    (setq tqnr-profile-ecb-history-make-buckets '())
    ) ;; (when tqnr-section-mode-cedet

  ;; BATCH: mode for .bat script in MS Windows
  (setq tqnr-section-mode-batch t)

  ;; VISUAL BASIC: mode for VisualBasic and VisualBasicAdvance
  (setq tqnr-section-mode-visual-basic t)

  ;; WINDOW NUMBERING: give a number of each window to easily jump in it
  (setq tqnr-section-mode-window-numbering nil)

  ;; CWARN
  ;; show small warning in code source
  ;; (ex: set in test, semi colon after test...)
  (setq tqnr-section-mode-cwarn nil)

  ;; SWIPER: Isearch alternative with fuzzy using ivy
  (setq tqnr-section-mode-swiper t)

  ;; ICOMPLETION: more completion in minibuffer
  (setq tqnr-section-mode-icompletion nil)

  ;; YASNIPPET: enable snippet for emacs
  (setq tqnr-section-mode-yasnippet nil)

  ;; BROWSE KILL RING: mode to browse the kill ring memory yank only on the first left top window...
  (setq tqnr-section-mode-browse-kill-ring t)

  ;; MM EOL: MAGNETI MARELLI, End Of Line file mode
  (setq tqnr-section-mode-mm-eol nil)

  ;; MM DBC: CAN dbc mode
  (setq tqnr-section-mode-mm-dbc nil)

  ;; MM DIFF: synergy classic diff mode
  (setq tqnr-section-mode-mm-diff nil)

  ;; DIRED PLUS: improve Dired mode, color, open with, etc
  (setq tqnr-section-mode-dired-plus t)

  ;; GNU GLOBAL: Tag management mode
  (setq tqnr-section-mode-gnu-global t)
  (when tqnr-section-mode-gnu-global
    ;; ggtags interface
    (setq tqnr-section-mode-gnu-global-ggtags t)
    ) ;; (when tqnr-section-mode-gnu-global

  ;; COUNSEL COMPILE: Use ivy to select a compile command.
  (setq tqnr-section-mode-counsel-compile t)

  ;; RTRT SCRIPT: rtrt script mode (syntax coloration)
  (setq tqnr-section-mode-rtrt-script nil)

  ;; VC CLEARCASE: vc ClearCase mode (not used)
  (setq tqnr-section-mode-vc-clearcase nil)

  ;; CLEARCASE: ClearCase mode
  (setq tqnr-section-mode-clearcase nil)

  ;; AUTOHOTKEY: AutoHotKey mode
  (setq tqnr-section-mode-autohotkey t)

  ;; OUTLINE: to manually hide some block in code source
  (setq tqnr-section-mode-outline t)
  (when tqnr-section-mode-outline
    ;; HIDE ALL AT START: hide all when opening file
    (setq tqnr-section-mode-outline-hide-all-at-start nil)
    ) ;; (when tqnr-section-mode-outline

  ;; AUTO HIGHLIGHT SYMBOL: to automatically highlight symbol at point
  (setq tqnr-section-mode-auto-highlight-symbol t)

  ;; GOOGLE CALENDAR: to import Google calendar
  (setq tqnr-section-mode-google-calendar nil)

  ;; FILL COLUMN INDICATOR: show a vertical line at fill-column column or customize it
  (setq tqnr-section-mode-fill-column-indicator t)
  (when tqnr-section-mode-fill-column-indicator
    ;; pixel width of vertical line default is 1 (nil)
    (setq tqnr-profile-fill-column-indicator-vertical-line-width nil)
    ;; color of vertical line in color format or nil (set comment theme face)
    (setq tqnr-profile-fill-column-indicator-vertical-line-color nil)
    ;; Use a fixed column for vertical line to not use fill-column value otherwise nil
    (setq tqnr-profile-fill-column-indicator-vertical-line-position nil)
    ;; enable vertical line in C mode
    (setq tqnr-profile-fill-column-indicator-mode-c nil)
    ;; enable vertical line in C++ mode
    (setq tqnr-profile-fill-column-indicator-mode-c++ nil)
    ;; enable vertical line in ADA mode
    (setq tqnr-profile-fill-column-indicator-mode-ada t)
    ;; enable vertical line in all mode
    (setq tqnr-profile-fill-column-indicator-mode-all nil)
    ) ;; (when tqnr-section-mode-fill-column-indicator

  ;; MUSE: muse mode to have nice doc
  (setq tqnr-section-mode-muse nil)

  ;; UNDO TREE: replace the undo built in function
  (setq tqnr-section-mode-undo-tree nil)

  ;; CSV: parse/edit/sort CSV file
  (setq tqnr-section-mode-csv t)

  ;; SUBVERSION: support Subversion 1.7
  (setq tqnr-section-mode-subversion nil)

  ;; DIFF COLOR: add color to diff mode
  (setq tqnr-section-mode-diff-color t)

  ;; DIRED SORT: more option to sort in Dired mode
  (setq tqnr-section-mode-dired-sort t)

  ;; ORG MODE: to organize everything (also use on Android)
  (setq tqnr-section-mode-org-mode t)
  (when tqnr-section-mode-org-mode
    ;; set org directory where every org file will goes
    (setq tqnr-profile-org-directory "U:/org")
    ;; default org file where all task/todo capture will goes
    (setq tqnr-profile-org-default-notes-file (concat (file-name-as-directory tqnr-profile-org-directory) "THALES.org"))
    ;; agenda will look only in default org file
    (setq tqnr-profile-org-agenda-files (concat (file-name-as-directory tqnr-profile-org-directory) "agenda.list"))
    ;; first buffer to show up is default org file when opening emacs
    (setq tqnr-section-mode-org-default-as-init-buffer t)
    ) ;; (when tqnr-section-mode-org-mode

  ;; ISEARCH PLUS: add some features to isearch
  (setq tqnr-section-mode-isearch-plus nil)

  ;; PSVN: add an icon in modeline where color give status of SVN
  (setq tqnr-section-mode-psvn nil)

  ;; POWERLINE: fancy modeline
  (setq tqnr-section-mode-powerline t)

  ;; NYAN: add bar in modeline given position in buffer
  (setq tqnr-section-mode-nyan nil)

  ;; SML: show position in modeline as a scrollbar
  (setq tqnr-section-mode-sml nil)

  ;; DIRED: change option to command ls for dired mode
  (setq tqnr-section-mode-dired t)

  ;; ISEARCH: scroll is possible when incremental search
  (setq tqnr-section-mode-isearch t)

  ;; RAINBOW DELIMITERS: highlight nested parentheses, brackets in different color depending of depth
  (setq tqnr-section-mode-rainbow-delimiters nil)

  ;; DIRED DETAILS: show hide details in dired mode
  (setq tqnr-section-mode-dired-details t)

  ;; SMART TAB: expand or indent at the point with tab
  (setq tqnr-section-mode-smart-tab nil)

  ;; FOLD DWIM: show hide code source block
  (setq tqnr-section-mode-fold-dwim t)

  ;; DIRED LIS: activate by default isearch in dired mode
  (setq tqnr-section-mode-dired-lis nil)

  ;; NXHTML: nXhtml: enhance html mode
  (setq tqnr-section-mode-nxhtml nil)

  ;; FASTNAV: fast navigation like with zap-to-char but only to move
  (setq tqnr-section-mode-fastnav nil)

  ;; MRU YANK: (Most Recently Used) in kill-ring
  (setq tqnr-section-mode-mru-yank t)

  ;; ACK: search with ack (no more grep) (need perl interpreter)
  (setq tqnr-section-mode-ack nil)

  ;; ACE JUMP
  ;; move quickly and easily with ace jump
  ;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
  (setq tqnr-section-mode-ace-jump nil)

  ;; AVY: move quickly and easily with avy (replacement of ace jump)
  (setq tqnr-section-mode-avy t)

  ;; DIREDFUL: color dired buffer
  (setq tqnr-section-mode-diredful nil)

  ;; PS2PDF
  ;; print buffer/region in pdf (the pdf background is unavoidably white so dark
  ;; theme don't render good)
  (setq tqnr-section-mode-ps2pdf nil)

  ;; AUCTEX: latex mode
  (setq tqnr-section-mode-auctex nil)

  ;; YASCROLL
  ;; add a small visual scroll-bar (can not be used with mouse click)
  ;; see https://github.com/m2ym/yascroll-el for screenshot
  (setq tqnr-section-mode-yascroll t)
  (when tqnr-section-mode-yascroll
    ;; time before hide scroll-bar (nil to always show)
    (setq tqnr-profile-yascroll-delay-to-hide nil)
    ) ;; (when tqnr-section-mode-yascroll

  ;; SMART FORWARD: move in code with semantic see example in plugins/smart-forward.el
  (setq tqnr-section-mode-smart-forward nil)

  ;; RAINBOW: show string color in color
  (setq tqnr-section-mode-rainbow t)

  ;; EDIFF: graphical diff (## to toggle whitespace ignoring)
  (setq tqnr-section-mode-ediff t)

  ;; MAGIT: use git with nice interface (do not use vc interface from emacs)
  (setq tqnr-section-mode-magit t)

  ;; SYNERGY: use synergy without java client GUI (do not use vc interface from emacs)
  (setq tqnr-section-mode-synergy nil)
  (when tqnr-section-mode-synergy
    ;; login to connect to synergy server
    (setq tqnr-profile-synergy-username "")
    ;; database path to connect to synergy server
    (setq tqnr-profile-synergy-database "")
    ;; server url to connect to synergy server
    (setq tqnr-profile-synergy-server "")
    ;; command line to modify history output
    ;; by example: '("|" "sed" "s/login/readable_name/")
    (setq tqnr-profile-synergy-history-filter '())
    ;; external tool to do diff with synergy
    (setq tqnr-profile-synergy-diff-external-command "BCompare.exe")
    ;; command line parameter to external tool to do diff
    (setq tqnr-profile-synergy-diff-external-parameter "")
    ;; swap files in diff (left/right) about external diff tool
    (setq tqnr-profile-synergy-diff-external-swap-file t)
    ) ;; (when tqnr-section-mode-synergy

  ;; HIDE LINES: hide lines using regexp (like narrow but with regex and not region)
  (setq tqnr-section-mode-hide-lines t)

  ;; AGGRESSIVE INDENT: indent all line in function/condition in C or lisp mode when edit it
  (setq tqnr-section-mode-aggressive-indent nil)

  ;; PLATINUM SEARCHER: A front-end for pt, The Platinum Searcher (faster than ack)
  (setq tqnr-section-mode-platinum-searcher nil)
  (when tqnr-section-mode-platinum-searcher
    ;; path to pt executable
    (setq tqnr-profile-mode-platinum-searcher-exec "pt.exe")
    ) ;; (when tqnr-section-mode-platinum-searcher

  ;; POPWIN: A pop-up manager for annoying buffer (have like ECB compilation buffer)
  (setq tqnr-section-mode-popwin t)

  ;; PROJECTILE: Project management, filtered find-file, only with root file from version control
  (setq tqnr-section-mode-projectile t)
  (when tqnr-section-mode-projectile
    ;; use fd to index project files
    (setq tqnr-profile-projectile-use-fd nil)
    ) ;; (when tqnr-section-mode-projectile

  ;; COMPANY: Completion mode using external back-ends to have symbol
  (setq tqnr-section-mode-company t)

  ;; EXPAND REGION: Increase selected region by semantic units
  (setq tqnr-section-mode-expand-region t)

  ;; FUNCTION ARGS: Show function parameters in C and C++
  (setq tqnr-section-mode-function-args nil)

  ;; ELPY
  ;; Python mode like an IDE (only install is from package)
  ;;  ;; add elpy package
  ;;  ;; and flycheck package, about warnings/errors check on the fly
  ;;  ;; and autopep8 package, about fix automagically some pep8 rules after save python file
  ;;  (add-to-list 'profile-environment-elpa-package-list 'elpy t)
  ;;  (add-to-list 'profile-environment-elpa-package-list 'flycheck t)
  ;;  (add-to-list 'profile-environment-elpa-package-list 'py-autopep8 t)
  (setq tqnr-section-mode-elpy t)
  (when tqnr-section-mode-elpy
    ;; override path for created elpy virtualenv (should have rights to execute)
    (setq tqnr-section-mode-elpy-rpc-virtualenv-path (concat (file-name-as-directory tqnr-dotemacs-path) "elpy"))
    ) ;; (when tqnr-section-mode-elpy

  ;; SMARTPARENS: useful to have nice navigation through source code structure
  (setq tqnr-section-mode-smartparens t)

  ;; PLANTUML: generate uml diagram from text
  (setq tqnr-section-mode-plantuml t)

  ;; GRAPHVIZ DOT: generate diagram from text
  (setq tqnr-section-mode-graphviz-dot nil)

  ;; HASKELL: editing, debugging and developing Haskell programs
  (setq tqnr-section-mode-haskell nil)

  ;; CFLOW: useful to have call tree in C source code
  (setq tqnr-section-mode-cflow nil)

  ;; IRONY: improving the editing experience for the C, C++ and Objective-C using clang
  (setq tqnr-section-mode-irony nil)

  ;; MARKDOWN: mode to edit Markdown-formatted text (by example wiki of github)
  (setq tqnr-section-mode-markdown t)
  (when tqnr-section-mode-markdown
    ;; to enable markdown mode with github flavoured for all .md files and not
    ;; only for README.md
    (setq tqnr-section-mode-markdown-github t)
    ) ;; (when tqnr-section-mode-markdown

  ;; EASY KILL: mode to easy copy/kill/cut text/line/word/expression/function...
  (setq tqnr-section-mode-easy-kill t)

  ;; ARDUINO: mode to enable c mode for .ino files and use emacs as external editor of arduino ide
  (setq tqnr-section-mode-arduino nil)

  ;; ALL THE ICONS
  ;; mode to have nice icons (from special fonts)
  ;; install font on your system from `fonts' folder or use
  ;; M-x all-the-icons-install-fonts
  (setq tqnr-section-mode-all-the-icons nil)

  ;; SHACKLE
  ;; mode to have popup always following same rules
  ;; like popwin but just add constraint to popup not replace the whole thing
  ;; Helm does not like popwin...
  (setq tqnr-section-mode-shackle t)

  ;; RIPGREP
  ;; A front-end for rg, ripgrep (faster than anything...)
  ;; use .ripgreprc to add new type
  (setq tqnr-section-mode-ripgrep t)

  ;; HYDRA: Create families of short bindings with a common prefix
  (setq tqnr-section-mode-hydra t)
  (when tqnr-section-mode-hydra
    ;; Use Hydra to manage rectangle shortcuts
    (setq tqnr-section-mode-hydra-rectangle t)
    ;; Use Hydra to manage windows/frame/buffer shortcuts
    (setq tqnr-section-mode-hydra-display t)
    ;; Use Hydra to manage transpose shortcuts
    (setq tqnr-section-mode-hydra-transpose t)
    ;; Use Hydra to manage help/web shortcuts
    (setq tqnr-section-mode-hydra-help-web t)
    ;; Use Hydra to manage macro shortcuts
    (setq tqnr-section-mode-hydra-macro t)
    ;; Use Hydra to manage spelling shortcuts
    (setq tqnr-section-mode-hydra-spelling t)
    ;; Use Hydra to manage search shortcuts
    (setq tqnr-section-mode-hydra-search t)
    ;; Use Hydra to manage smartparens shortcuts
    (setq tqnr-section-mode-hydra-smartparens t)
    ;; Use Hydra to manage ada compile shortcuts
    (setq tqnr-section-mode-hydra-ada nil)
    ;; Use Hydra to manage outline shortcuts
    (setq tqnr-section-mode-hydra-outline t)
    ;; Use Hydra to manage org shortcuts
    (setq tqnr-section-mode-hydra-org-mode t)
    ;; Use Hydra to manage special buffer toggle shortcuts
    (setq tqnr-section-mode-hydra-special-buffer t)
    ) ;; (when tqnr-section-mode-hydra

  ;; FLYSPELL: On-the-fly spell checking
  (setq tqnr-section-mode-flyspell nil)
  (when tqnr-section-mode-flyspell
    ;; set program to be use with ispell
    (setq tqnr-profile-ispell-program "aspell")
    ;; language to use with ispell
    (setq tqnr-profile-ispell-dictionary "english")
    ;; POPUP: Correct the misspelled word in popup menu
    (setq tqnr-section-mode-flyspell-popup t)
    ) ;; (when tqnr-section-mode-flyspell

  ;; DUMB JUMP: On-the-fly spell checking
  (setq tqnr-section-mode-dumb-jump t)

  ;; ADA: Ada mode for edit/navigate/compile ada source code
  (setq tqnr-section-mode-ada t)

  ;; FITNESSE: FitNesse MarkUp files syntax highlight
  (setq tqnr-section-mode-fitnesse t)

  ;; PANDOC: PanDoc tools mode to translate between markup syntax
  (setq tqnr-section-mode-pandoc t)

  ;; FLEX ISEARCH: Flex Isearch mode add fuzzy match when doing incremental search
  (setq tqnr-section-mode-flex-isearch nil)

  ;; ORG JIRA: Sync JIRA issues with org-mode issues
  (setq tqnr-section-mode-org-jira t)

  ;; GNUPLOT: Major mode for editing gnuplot scripts
  (setq tqnr-section-mode-gnuplot t)

  ;; POWERSHELL: Powershell mode
  (setq tqnr-section-mode-powershell t)

  ;; HELPFUL: Helpful mode
  (setq tqnr-section-mode-helpful t)

  ;; IALIGN: ialign package
  (setq tqnr-section-mode-ialign t)

  ;; REALGUD: realgud mode
  (setq tqnr-section-mode-realgud t)

  ;; MOVE TEXT: realgud mode
  (setq tqnr-section-mode-move-text t)

  ;; GROOVY: groovy mode
  (setq tqnr-section-mode-groovy t)

  ;; UNDO FU: replace the undo built in function without same problem than undo-tree
  (setq tqnr-section-mode-undo-fu t)

  ;; UNDO FU SESSION: Save & recover undo steps between Emacs sessions
  (setq tqnr-section-mode-undo-fu-session nil)
  (when tqnr-section-mode-undo-fu-session
    ;; set undo fu session directory where every undo will be stored
    (setq tqnr-profile-undo-fu-session-directory (concat (file-name-as-directory tqnr-dotemacs-path) "undo"))
    ;; set file size limit about session save file
    (setq tqnr-profile-undo-fu-session-file-limit 1024)
    ) ;; (when tqnr-section-mode-undo-fu-session

  ;; HEADER2: Create/Update header of files
  (setq tqnr-section-mode-header2 t)

  ;; LSP MODE: Lsp mode
  (setq tqnr-section-mode-lsp-mode nil)

  ;; LUA MODE: Lua mode
  (setq tqnr-section-mode-lua-mode nil)

  ;; PROJECTILE DIRENV: Projectile direnv mode to set environment variable as found in direnv config
  (setq tqnr-section-mode-projectile-direnv t)

  ;; DIMINISH: shrink major and minor mode name in the modeline
  (setq tqnr-section-mode-diminish nil)
  ) ;; (when tqnr-section-mode

;; LANGUAGES: Set style and/or indentation for multiple languages
(setq tqnr-section-languages t)
(when tqnr-section-languages
  ;;
  ;; C
  ;;   language settings, set indentation style and preprocessing option
  (setq tqnr-section-languages-c t)
  (when tqnr-section-languages-c
    ;; number of space for indentation in C
    (setq tqnr-profile-c-indent-offset 2)
    ;; new types (add name string in list)
    (setq tqnr-profile-c-extra-types
      '(
         "t_u8"
         "t_i8"
         "t_u16"
         "t_i16"
         "t_u32"
         "t_i32"
         "t_u64"
         "t_i64"
         )
      ) ;; (setq tqnr-profile-c-extra-types
    ;; Compile mode without ask
    (setq tqnr-profile-c-ask-before-compile t)
    ;; INDENT PREPROCESSOR: make a #define be align with C code
    (setq tqnr-section-languages-c-indent-preprocessor nil)
    ;; FLYMAKE: verification error/warning in source code on the fly
    (setq tqnr-section-languages-c-flymake nil)
    ;; FLYCHECK: verification error/warning in source code on the fly
    (setq tqnr-section-languages-c-flycheck t)
    ;; command to preprocess
    (setq tqnr-profile-c-macro-preprocessor "cpp -C")
    ;; set flags about macro preprocessing
    (setq tqnr-profile-c-macro-cppflags "-D__DEBUG__")
    ) ;; (when tqnr-section-languages-c
  ;;
  ;; LISP
  ;;   set indentation style
  (setq tqnr-section-languages-lisp t)
  (when tqnr-section-languages-lisp
    ;; number of space for indentation in lisp
    (setq tqnr-profile-lisp-indent-offset 2)
    ) ;; (when tqnr-section-languages-lisp
  ;;
  ;; TABULATION
  ;;   tab always in space
  (setq tqnr-section-languages-tabulation t)
  ;;
  ;; RTRT SCRIPT PTU
  ;;   set indentation style
  (setq tqnr-section-languages-rtrt-script nil)
  (when tqnr-section-languages-rtrt-script
    ;; set number of space for indentation in rtrt script .ptu
    (setq tqnr-profile-rtrt-indent-offset 2)
    ) ;; (when tqnr-section-languages-rtrt-script
  ;;
  ;; PERL
  ;;   set indentation style
  (setq tqnr-section-languages-perl t)
  (when tqnr-section-languages-perl
    ;; number of space for indentation in perl
    (setq tqnr-profile-perl-indent-offset 2)
    ) ;; (when tqnr-section-languages-perl
  ;;
  ;; C++ QT
  ;;   set include for Qt 4.8
  (setq tqnr-section-languages-c++-qt nil)
  ;;
  ;; ARDUINO
  ;;   set indentation style
  (setq tqnr-section-languages-arduino t)
  (when tqnr-section-languages-arduino
    ;; number of space for indentation in Arduino
    (setq tqnr-profile-arduino-indent-offset 2)
    ) ;; (when tqnr-section-languages-arduino
  ) ;; (when tqnr-section-languages

;; SELECTION: selection can be kill + selection is highlight + kill->copy in read only
(setq tqnr-section-selection t)
(when tqnr-section-selection
  ;; SHIFT SELECTION
  (setq tqnr-section-selection-with-shift nil)
  ) ;; (when tqnr-section-selection

;; DISPLAY: modification about display in buffers, font, color...
(setq tqnr-section-display t)
(when tqnr-section-display

  ;; BUFFER
  ;; buffers with *buffername* should be displayed in the same window
  ;; first column in window will display buffer limit, next page will leave 5 shared line
  (setq tqnr-section-display-buffer t)
  (when tqnr-section-display-buffer
    ;; VISUAL LINE: word wrap, truncate line without cut word
    ;; END and HOME will go to the end/start of screen line not logical line
    (setq tqnr-section-display-windows-buffers-visual-line nil)
    ) ;; (when tqnr-section-display-buffer

  ;; SPEEDBAR: set size and display of speedbar (see GLOSSARY) (no used)
  (setq tqnr-section-display-speedbar nil)

  ;; FONT: set font in terminal or in graphical
  (setq tqnr-section-display-font t)
  (when tqnr-section-display-font
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
    ;;(setq tqnr-profile-font "Iosevka-8")
    (setq tqnr-profile-font "Iosevka NF-8")
    ;; ANTIALIAS: set antialiasing on font rendering
    (setq tqnr-section-display-font-antialias t)
    ) ;; (when tqnr-section-display-font

  ;; COLOR: set color in emacs
  (setq tqnr-section-display-color t)
  (when tqnr-section-display-color
    ;; PARENTHESES MODE: matched parentheses are highlight
    (setq tqnr-section-display-color-parentheses-mode t)
    ;; PARENTHESES MINIBUFFER: matched parentheses are highlight and if not
    ;; visible show it in the Minibuffer
    (setq tqnr-section-display-color-parentheses-visible nil)
    ;; PARENTHESES HIGHLIGHT: matched parentheses are highlight in rainbow color
    (setq tqnr-section-display-color-parentheses-highlight nil)
    ;; COLOR THEME: set color by color-theme mode (or manual settings nil)
    (setq tqnr-section-display-color-theme t)
    ;; theme to be used, do not use it with terminal
    ;; (setq tqnr-profile-color-theme "solarized-light") ;; nice light theme
    ;;(setq tqnr-profile-color-theme "base16-atlas") ;; too blue + low contrast
    ;;(setq tqnr-profile-color-theme "base16-chalk") ;; comment to dark and ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-circus") ;; comment to dark and ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-classic-dark") ;; nice but a little too low constrast + ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-darktooth") ;; nice but too low contrast specialy function color
    ;; (setq tqnr-profile-color-theme "base16-default-dark") ;; very nice but ada too purple/blue (maybe switch some color)
    ;;(setq tqnr-profile-color-theme "base16-dracula") ;; too pinky
    ;; (setq tqnr-profile-color-theme "base16-eighties") ;; very nice but ada too purple/blue (maybe switch some color)
    ;;(setq tqnr-profile-color-theme "base16-ia-dark") ;; nice but comment start not same color than rest of comment + ada too purple/blue (maybe switch some color)
    ;;(setq tqnr-profile-color-theme "base16-material-darker") ;; very nice but comment a little too low contrast and ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-mocha") ;; too low contrast
    ;;(setq tqnr-profile-color-theme "base16-monokai") ;; nice, vivid color but background too dirty and ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-oceanicnext") ;; nice but background too blue + ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-ocean") ;; too low contrast and too blue
    ;;(setq tqnr-profile-color-theme "base16-onedark") ;; nice but comment hard to read + ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-railscasts") ;; nice even with ada purple/blue
    ;; (setq tqnr-profile-color-theme "base16-snazzy") ;; nice vivid colors but start of comment not same color + ada too pink/cyan
    ;;(setq tqnr-profile-color-theme "base16-solarflare") ;; nice color but selection unreadable + ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-spacemacs") ;; color too vivid then hard to read + ada too purple/blue
    ;;(setq tqnr-profile-color-theme "base16-tomorrow-night") ;; nice but low constrast + start of comment not same color + ada purple/blue
    ;;(setq tqnr-profile-color-theme "base16-tomorrow") ;; light theme
    ;;(setq tqnr-profile-color-theme "base16-twilight") ;; too low contrast
    ;;(setq tqnr-profile-color-theme "base16-woodland") ;; nice but start of comment not same color + ada purple/blue
    ;;(setq tqnr-profile-color-theme "base16-zenburn") ;; nice but a little too low contrast + ada pink/cyan
    ;;(setq tqnr-profile-color-theme "doom-dracula") ;; nice but background/comment too blue
    ;;(setq tqnr-profile-color-theme "doom-molokai") ;; nice but a little pinky
    ;;(setq tqnr-profile-color-theme "doom-nord") ;; blueish and too low contrast
    ;; (setq tqnr-profile-color-theme "doom-nova") ;; nice even with ada purple/blue, modeline to revise
    ;;(setq tqnr-profile-color-theme "doom-one") ;; nice but too low contrast and ada too purple/blue
    ;;(setq tqnr-profile-color-theme "doom-opera") ;; nice need more vivid + selection not distinguishable + some background change (example parenthesis match) + ada not too purple/blue !
    ;;(setq tqnr-profile-color-theme "doom-peacock") ;; nice but some purple in modeline... + pink in helm
    ;;(setq tqnr-profile-color-theme "doom-tomorrow-night") ;; too low contrast an ada too purple/blue + some background change (example parenthesis match)
    ;;(setq tqnr-profile-color-theme "atom-one-dark") ;; nice but a little too blueish
    ;;(setq tqnr-profile-color-theme "badger") ;; nice but remove bold and need update for a lot of package
    ;;(setq tqnr-profile-color-theme "challenger-deep") ;; nice but wrong background and powerline
    ;; (setq tqnr-profile-color-theme "colorsarenice-dark") ;; very nice still too much purple/pink in ada
    ;;(setq tqnr-profile-color-theme "flatland-black") ;; like default dark color but with MS Windows XP colors
    ;;(setq tqnr-profile-color-theme "flatland") ;; like default dark color but with MS Windows XP colors
    ;; (setq tqnr-profile-color-theme "gruvbox-dark-hard") ;; very nice color but wrong powerline color
    ;; (setq tqnr-profile-color-theme "gruvbox-dark-medium") ;; very nice color but wrong powerline color
    ;;(setq tqnr-profile-color-theme "gruvbox-dark-soft") ;; nice color but too low contrast (background not dark enough)
    ;;(setq tqnr-profile-color-theme "jazz") ;; nice color but too low contrast and too dark (but ada not purple/blue/pink)
    ;;(setq tqnr-profile-color-theme "labburn") ;; like zenburn too low contrast (but add not purple/blue/pink)
    ;;(setq tqnr-profile-color-theme "matrix") ;; almost only green (ada is like without syntax color)
    ;;(setq tqnr-profile-color-theme "molokai") ;; vivid color, comment color near background (ada too cyan/pink) (powerline ok)
    ;;(setq tqnr-profile-color-theme "monokai") ;; vivid color, pink too saturated (ada pink/green/cyan) (ugly powerline color)
    ;;(setq tqnr-profile-color-theme "mustard") ;; nice gradation of yellow but use underline...
    ;;(setq tqnr-profile-color-theme "obsidian") ;; too blueish with bold and without color for function (ada only green and white)
    ;; (setq tqnr-profile-color-theme "railscasts-reloaded") ;; nice gradation of brown (powerline not link to brownish)
    ;;(setq tqnr-profile-color-theme "railscasts") ;; better gradation of brown (powerline not link to brownish)
    ;;(setq tqnr-profile-color-theme "rippedcasts") ;; same than railcasts (wrong cursor and powerline colors)
    ;; (setq tqnr-profile-color-theme "sanityinc-tomorrow-bright") ;; nice very color dark background (ada not purple/blue/pink)
    ;; (setq tqnr-profile-color-theme "sanityinc-tomorrow-eighties") ;; very nice color a little too low contrast
    (setq tqnr-profile-color-theme "sanityinc-tomorrow-dark") ;; almost perfect (need to custom powerline color)
    ;; (setq tqnr-profile-color-theme "smyx") ;; nice color but box and bold are used (need to custom powerline color)
    ;;(setq tqnr-profile-color-theme "spolsky") ;; nice colorful but too light on mode support
    ;;(setq tqnr-profile-color-theme "tango-2") ;; nice tango color (powerline color to revise) and too light on mode support
    ;;(setq tqnr-profile-color-theme "tangotango") ;; nice tango color, bold... and too light on mode support
    ;;(setq tqnr-profile-color-theme "warm-night") ;; low contrast and fade color but use bold and box
    ;;(setq tqnr-profile-color-theme "wilson") ;; nice but sad color like an raining day
    ;;(setq tqnr-profile-color-theme "yoshi") ;; nice low contrast and fade color but light mode support (wrong powerline)
    ;;(setq tqnr-profile-color-theme "zen-and-art") ;; nice color but comment too dark and very light mode support
    ;;(setq tqnr-profile-color-theme "zenburn") ;; well known low contrast and good color (powerline not very readable)

    ;; useful to not have bold or italic in doom theme
    (custom-set-variables
      '(doom-themes-enable-bold nil)
      '(doom-themes-enable-italic nil)
      )
    ;; ANSI COLOR COMPILE WINDOW: have color and no more junk like this ^[[32m
    (setq tqnr-section-display-color-ansi-color-compile t)
    ;; HIGHLIGHT CURRENT LINE: have current line highlighted
    (setq tqnr-section-display-color-highlight-line nil)
    ) ;; (when tqnr-section-display-color
  ) ;; (when tqnr-section-display

;; INTERFACE: modification of GNU Emacs interface, size, title, decoration...
(setq tqnr-section-interface t)
(when tqnr-section-interface

  ;; MAIN WINDOW: modification about main window of emacs
  (setq tqnr-section-interface-main-window t)
  ;;
  (when tqnr-section-interface-main-window
    ;; DECORATION: remove all mouse interface (toolbar, menubar, scrollbar)
    (setq tqnr-section-interface-remove-decoration t)
    ;;
    ;; WINDOW TITLE: buffer name in title bar (example "< foobar.c >") (from grandm_y)
    ;; %b buffername ; %F frame name ; %l line number ; %c column number
    ;; %p percent of buffer above top ; %m mode name ; %n Narrow mode
    ;; %z coding systems ; %Z %z + end-of-line format ; %- infinitely dashes
    (setq tqnr-profile-window-title "GNU Emacs - %b")
    ;;
    ;; TRANSPARENCY: the whole emacs window will be transparent
    (setq tqnr-section-interface-transparency nil)
    ;; transparency of the window. 0=transparent/100=opaque
    (setq tqnr-profile-transparency 96)
    ;;
    ;; FULLSCREEN: main window start in fullscreen
    (setq tqnr-section-interface-fullscreen t)
    ;;
    ;; Do not popup any window by splitting vertically only horizontally
    (setq tqnr-section-interface-popup-window-horizontally t)
    ) ;; (when tqnr-section-interface-main-window

  ;; MODELINE
  ;; set some option to add in the grey line at the bottom of each buffer
  ;; (replaced by powerline mode)
  (setq tqnr-section-interface-modeline t)

  ;; ECB: set size, display, refresh and remove opening tips of ECB window
  (setq tqnr-section-interface-ecb nil)
  (when tqnr-section-interface-ecb
    ;; ECB ICON FOR TREE: display icon image instead of ascii guides for arborescence tree
    (setq tqnr-section-interface-ecb-ascii-tree nil)
    ) ;; (when tqnr-section-interface-ecb
  ) ;; (when tqnr-section-interface

;; COMPLETION: enable letter case completion + dynamic completion
(setq tqnr-section-completion t)

;; SHORTCUT: custom binding or shortcut for everything in GNU Emacs (thought for qwerty keyboard)
(setq tqnr-section-shortcut t)
(when tqnr-section-shortcut

  ;; GLOBAL: add global shortcut (for whole Emacs)
  (setq tqnr-section-shortcut-global t)
  (when tqnr-section-shortcut-global
    ;; CUA: enable C-x, C-c, C-v to cut copy paste
    ;; don't recommend it otherwise see http://www.emacswiki.org/CuaMode
    (setq tqnr-section-shortcut-global-cua nil)
    ) ;; (when tqnr-section-shortcut-global

  ;; WINDOWS: add shortcut to manage windows
  (setq tqnr-section-shortcut-windows t)

  ;; BUFFER: add shortcut to manage buffers
  (setq tqnr-section-shortcut-buffer t)

  ;; GREP: add shortcut to manage grep
  (setq tqnr-section-shortcut-grep t)

  ;; ETAG: add shortcut to manage gtags or etags
  (setq tqnr-section-shortcut-etag t)

  ;; SEMANTIC: add shortcut to move in source code with semantic
  (setq tqnr-section-shortcut-semantic t)

  ;; HOOK: run hook for all shortcuts from the whole configuration
  (setq tqnr-section-shortcut-hook t)
  ) ;; (when tqnr-section-shortcut

;; MOUSE: smooth wheel + lazy decoration when scroll
(setq tqnr-section-mouse t)
(when tqnr-section-mouse
  ;; PASTE CURSOR: yank at point and not at mouse cursor (either when yank with mouse wheel)
  (setq tqnr-section-mouse-paste-to-point-not-mouse-cursor t)
  ;; AVOIDANCE: move mouse cursor at top right of the buffer to not bother me
  (setq tqnr-section-mouse-avoidance t)
  ;; SMOOTH SCROLL: scroll with margin and without jump
  (setq tqnr-section-mouse-smooth-scroll nil)
  ) ;; (when tqnr-section-mouse

;; ANNOYANCES
;; no welcome message + yes->y + do not query to refresh buffer + remove insert
;; key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog box + no
;; tooltips
(setq tqnr-section-annoyances t)
(when tqnr-section-annoyances
  ;; ask confirmation to quit Emacs
  (setq tqnr-section-annoyances-comfirm-quit t)
  ;;
  ;; TRUNCATE LINE: whole line not visible (need to scroll right)
  (setq tqnr-section-annoyances-truncate-line t)
  ;;
  ;; SCROLL PRESERVE CURSOR POSITION: when wheel scroll the cursor do not move
  (setq tqnr-section-annoyances-scroll-preserve-cursor-position t)
  ;;
  ;; NO BACKUP FILE: turn off backup files
  (setq tqnr-section-annoyances-no-backup-file t)
  ;;
  ;; ALL BACKUP FILE IN DIRECTORY: all backup files goes in a directory
  (setq tqnr-section-annoyances-backup-file-in-directory nil)
  (when tqnr-section-annoyances-backup-file-in-directory
    ;; all backup files goes in a directory
    (setq tqnr-profile-backup-directory "")
    (setq tqnr-profile-autosave-directory "")
    ) ;; (when tqnr-section-annoyances-backup-file-in-directory
  ) ;; (when tqnr-section-annoyances

;; MISC
;; remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
(setq tqnr-section-misc t)
(when tqnr-section-misc
  ;;
  ;; SPACE: remove useless space at the end of line
  (setq tqnr-profile-remove-useless-ending-space t)
  ;; END OF FILE: be sure that a new line is at the end of a file when it's saved
  (setq tqnr-profile-always-new-line-at-end t)
  ;; COLUMN: fill-xxx is set with a width
  (setq tqnr-profile-fill-column 80)
  ;;
  ;; WEB: set browser to open url
  (setq tqnr-profile-browser "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
  ;;
  ;; CALENDAR: set latitude/longitude + location + holidays + custom date in
  ;; Modeline lunar phase, sunrise/sunset, time, etc
  (setq tqnr-section-misc-calendar nil)
  (when tqnr-section-misc-calendar
    ;; FRENCH CALENDAR: set French holidays and day/month/moon phase name
    (setq tqnr-section-misc-calendar-french nil)
    ) ;; (when tqnr-section-misc-calendar
  ;; BOOKMARK: default file, each command to add/modify bookmark save bookmark file
  (setq tqnr-section-misc-bookmark t)
  (when tqnr-section-misc-bookmark
    ;; BOOKMARK SORT: sort or do not sort bookmark when saving bookmark file
    (setq tqnr-profile-bookmark-sort nil)
    ) ;; (when tqnr-section-misc-bookmark t
  ;; SCREENSAVER: when idle for 5min some animations on buffer text
  (setq tqnr-section-misc-screensaver nil)
  ) ;; (when tqnr-section-misc

;; SAFE LOCAL VARIABLE: all customize settings from any .dir-local.el are put in customize
(setq tqnr-section-safe-local-variable t)

;; FILECUSTOMIZE
;; all customize settings are put in here when you use interface
;; (customize) to change settings
(setq tqnr-section-filecustomize t)

;; AFTER LOADING CONF
;; this function will be call at the end after all configuration, it can be use
;; to override some settings or add settings without modify the configuration
(defun function-to-call-after-loading-conf ()
  ;; to disable all version control
  (setq vc-handled-backends nil)

  ;; U: is thales server home, it contains a function called by .dir-locals.el of some project
  ;; then avoid ask each time it is modify
  (add-to-list 'load-path "u:/")
  (try-require 'project-env "")

  ;; set only one zone program
  (setq zone-programs [ zone-pgm-putz-with-case ])

  ;; try to unslow emacs when open file
  (remove-hook 'find-file-hooks 'vc-find-file-hook)

  ;;(setq dumb-jump-rg-cmd (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/ripgrep-x86_64/rg.exe"))
  ;; useful to let mega large project
  (setq dumb-jump-max-find-time 30)

  ;; do not freeze when redisplay ?
  ;;(redisplay-dont-pause t)

  ;; Scrolling.
  ;; Good speed and allow scrolling through large images (pixel-scroll).
  ;; Note: Scroll lags when point must be moved but increasing the number
  ;;       of lines that point moves in pixel-scroll.el ruins large image
  ;;       scrolling. So unfortunately I think we'll just have to live with
  ;;       this.
  (pixel-scroll-mode)
  (setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
  (setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
  (setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
  (setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.
  (setq fast-but-imprecise-scrolling t) ; No (less) lag while scrolling lots.
  (setq jit-lock-defer-time 0) ; Just don't even fontify if we're still catching up on user input.

  ;; do not go to warning in compilation mode
  (setq compilation-skip-threshold 2)

  ;; reduce magit slow on windows
  (setq w32-pipe-read-delay 0)

  ;; needed to avoid error in colors
  (powerline-reset)

  (message "### END")
  ) ; (defun function-to-call-after-loading-conf ()


(provide 'profile-thales-avionics-server)

;;; profile-thales-avionics-server.el ends here
