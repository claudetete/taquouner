;;; emacs.el --- config file for Emacs

;; Copyright (c) 2006-2017 Claude Tete
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
;; Version: 9.4
;; Created: October 2006
;; Last-Updated: May 2017

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
;;   Cygwin: Unix environment in a MS Windows world
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
;;   Mode: plug in, extension, add-on, of Emacs to add functionalities (ex:
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
;;
;;
;; DONE
;;  - adding a profile.el file with each configuration needed (maybe a
;;    profile-end.el to add some config without changing other file to have easy
;;    update)
;;  - try browse-kill-ring mode (do not work properly with ECB)
;;  - put all comments in English
;;  - try rect-mark mode/plug-in (use CUA rectangle mode)
;;  - make a option to have CEDET in Emacs or from repo
;;  - configure semantic
;;  -- use gcc to hide/show #ifdef... (can be done with EDE and semantic but
;;     does not work correctly (GNU Global fault)) but it is configured for
;;     preproc macro
;;  - manage properly windows or Linux
;;  - use gcc for preprocessing of ifdef etc
;;  - use GNU/Global to manage project and tag
;;  - try system of project/session (not used)
;;  - use pc-keys.el mode
;;  - put condition to make it parametrable
;;  - put message when loading
;;  - manage environment
;;  - switch buffer without all *buffer*
;;  - show current function in ECB methods window
;;  - put colors with grep (remove underline and put in grey background)
;;  - grep definition and declaration with default path
;;  - configure ECB + shortcut + bug opening new window
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
;;  - remove trailing space
;;  - use Dired+ mode to comfy browsing
;;  - add "end" and "home" x1, x2 and x3

;;; Change Log:
;; 2017-05-30 (9.4)
;;    add markdown mode
;; 2016-09-28 (9.3)
;;    add proxy for packages + new GNU Global mode + add platinium search,
;;    popwin, projectile, company and elpy mode
;; 2015-08-21 (9.2)
;;    add hide-lines mode + aggressive indent mode + remove old colors + custom
;;    hl-line
;; 2014-03-26 (9.1)
;;    add synergy-web mode and remove old function synergy
;; 2013-09-10 (9.0)
;;    add magit mode, ansi color mode + synergy functions
;; 2013-05-30 (8.9)
;;    add ediff section + remove old scroll section
;; 2013-05-17 (8.8)
;;    add rainbow mode + window title by profile + smooth scroll mode + clean
;;    section to load nothing when profile is empty
;; 2013-05-07 (8.7)
;;    add yascroll and smart forward mode + antialias for font + some minor
;;    options
;; 2013-02-05 (8.6)
;;    add auctex mode
;; 2012-12-05 (8.5)
;;    add compile command confirmation + show warning + clean up
;; 2012-11-29 (8.4)
;;    add ace jump + flymake mode
;; 2012-11-26 (8.3)
;;    add ack mode
;; 2012-10-31 (8.2)
;;    put server in environment file + add fastnav and mru yank mode + C
;;    language setting
;; 2012-10-18 (8.1)
;;    fix bug with server + add mode dired lis, nxhtml and config for C++ Qt
;; 2012-08-01 (8.0)
;;    add visual line mode
;; 2012-07-19 (7.9)
;;    add fold dwim mode
;; 2012-07-09 (7.8)
;;    remove extern files put in mode + functions alone + add rainbow delimiters
;; 2012-06-21 (7.7)
;;    add web browser
;; 2012-06-19 (7.6)
;;    fix bug with default font on linux + option for powerline + screensaver
;;    mode
;; 2012-06-13 (7.5)
;;    add new section about version recognition + scroll line by line +
;;    powerline + nyan bar + sml bar + diminish in modeline
;; 2012-05-29 (7.4)
;;    add condition to change clt-working-environment (profile) + enable server
;; 2012-05-25 (7.3)
;;    add isearch+ mode
;; 2012-05-11 (7.2)
;;    add hyper and super keys + org mode + fix some bugs
;; 2012-05-04 (7.1)
;;    add Dired sort mode + transparency setting + ECB ascii tree setting
;; 2012-05-02 (7.0)
;;    fix bug about fullscreen (must be set after a change of font) + add some
;;    mode
;; 2012-04-23 (6.9)
;;    add section mouse avoidance
;; 2012-04-19 (6.8)
;;    add dictionary
;; 2012-04-04 (6.7)
;;    add Perl language
;; 2012-04-01 (6.6)
;;    add elpa package manager + translate some comment in English + Google
;;    calendar
;; 2012-03-27 (6.5)
;;    add outline mode + auto highlight symbol minor mode
;; 2012-03-20 (6.4)
;;    add rtrt script mode + vc clearcase
;; 2012-03-03 (6.3)
;;    add a glossary + comment for each option
;; 2012-03-02 (6.2)
;;    add some option about CEDET and semantic
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
;;    add eproject, must try it without ECB
;; 2011-05-25 (5.3)
;;    add some option about tags
;; 2011-03-10 (5.2)
;;    split dot emacs file
;; 2010-11-19 (5.1)
;;    add new shortcut for align-regexp
;; 2010-11-09 (5.0)
;;    add mode yasnippet
;; 2010-11-03 (4.9)
;;    date in Minibuffer and in French format
;; 2010-11-02 (4.8)
;;    messages + condition (parametrable)
;; 2010-10-21 (4.7)
;;    number window navigation, brace func, size window
;; 2010-10-07 (4.6)
;;    some minor modification
;; 2010-10-05 (4.5)
;;    add semantic for ECB methods
;; 2010-09-16 (4.4)
;;    shortcut buffer + selectword + colors ECB
;; 2010-08-31 (4.3)
;;    utf-8 and function for grep + etags
;; 20s10-08-26 (4.2)
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
(message "--[ Loading my Emacs init file ]--")

(when (string-equal system-type "windows-nt")
  ;;;; remove error "directory ~/.emacs.d/server is unsafe"
  ;;;; but if you cannot be the owner of this directory.
  ;;(defun server-ensure-safe-dir (dir) "Noop" t)
  )

;;;; debug this file if error
;;;; do not always work and can be opened in use
;;(setq debug-on-error t)

;; increase the size of the log *Messages*
(custom-set-variables
  '(message-log-max 1000))

;;; attempt to load a feature/library, failing silently (by Fabrice Niessen)
;; patched by Claude TETE to add string before message
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature &optional indent)
  "Attempt to load a library or module (FEATURE).  Return true if the library \
given as argument is successfully loaded.  If not, instead of an error, just \
add the package to a list of missing packages. INDENT contains string to add \
before message."
  (let ()
    (if (eq indent nil) (setq indent ""))
  (condition-case err
      ;; protected form
      (progn
        (message "%sChecking for library `%s'..." indent feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "%sChecking for library `%s'... Found" indent feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "%sChecking for library `%s'... Missing" indent feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil))))


;;
;;; SETTINGS
;; (defvar section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' or 't' value
;;
;;; ENVIRONMENT                                                         0
;; FILE: dotemacs/environment.el
(defvar section-environment t)
(progn
  ;; PROFILE                                                            0.1
  (defvar section-environment-profile t)
  (progn
    (defvar profile-name "default")
    (defvar profile "Default")
    ) ; (progn
  ;;
  ;; VERSION RECOGNITION                                                0.2
  ;; detect system: Emacs version
  (defvar section-environment-version-recognition nil)
  ;;
  ;; OS RECOGNITION                                                     0.3
  (defvar section-environment-os-recognition nil)
  ;;
  ;; TERMINAL VS GRAPHICS                                               0.4
  (defvar section-environment-terminal-vs-graphics nil)
  ;;
  ;; SET PATH                                                           0.5
  ;; REQUIREMENT:       section-environment-os-recognition
  ;;                    section-environment-terminal-vs-graphics
  (defvar section-environment-set-path nil)
  (progn
    (defvar profile-path "")
    (defvar profile-exec-path '(""))
    (defvar profile-lang "en_US")
    ) ; (progn
  ;;
  ;; MS WINDOWS PERFORMANCE                                             0.6
  ;; REQUIREMENT:       section-environment-os-recognition
  ;; improve performance
  (defvar section-environment-ms-windows-performance nil)
  ;;
  ;; EXECUTABLE                                                         0.7
  ;; REQUIREMENT:       section-environment-os-recognition
  ;;                    section-environment-terminal-vs-graphics
  (defvar section-environment-executable nil)
  (progn
    (defvar profile-shell-file-name "bash")
    (defvar profile-ediff-diff-program "diff")
    (defvar profile-ediff-diff3-program "diff3")
    (defvar profile-ediff-cmp-program "cmp")
    ) ; (progn
  ;;
  ;; ELPA                                                               0.8
  ;; for multiple repo and up to date
  (defvar section-environment-elpa nil)
  (progn
    (defvar profile-environment-elpa-proxy-http nil)
    (defvar profile-environment-elpa-proxy-https nil)
    (defvar profile-environment-elpa-package-list '())
    )
  ;;
  ;; HYPER                                                              0.9
  ;; set menu key as hyper key
  (defvar section-environment-hyper nil)
  ;;
  ;; SUPER                                                              0.10
  ;; set windows key as super key
  (defvar section-environment-super nil)
  ;;
  ;; SERVER                                                              0.11
  ;; start a server for emacs client (and have only one instance)
  (defvar section-environment-server nil)
  ) ; progn


;;
;; FUNCTIONS                                                            1
;; FILE: dotemacs/functions.el
;; REQUIREMENT:       section-environment-os-recognition
;; load custom function
(defvar section-functions nil)
(progn
  ;; MAGNETI MARELLI                                                    1.1
  ;; FILE: dotemacs/function-mm.el
  ;; load custom function for MM profile
  (defvar section-function-mm nil)
  ) ; (progn


;;
;;; MODE                                                                2
;; FILE: dotemacs/mode.el
(defvar section-mode nil)
;; load modes in plugins/
(progn
  ;; DIRECTORY                                                          2.1
  ;; add "plugins/" to load path
  ;; needed for all `section-mode-*'
  (defvar section-mode-directory nil)
  ;;
  ;; VECTRA                                                             2.2
  ;; FILE: plugins/vectra.el
  ;; man and doc in emacs (never used)
  (defvar section-mode-vectra nil)
  ;;
  ;; HOME/END                                                           2.3
  ;; FILE: plugins/pc-keys.elc
  ;; add some useful function to home and end keys
  (defvar section-mode-home-end nil)
  ;;
  ;; DOXYMACS                                                           2.4
  ;; emacs interface for doxygen comments
  (defvar section-mode-doxymacs nil)
  ;;
  ;; IDO                                                                2.5
  ;; yet another switch buffer
  (defvar section-mode-ido nil)
  ;;
  ;; UNIQUIFY                                                           2.6
  ;; create unique buffer name
  (defvar section-mode-uniquify nil)
  ;;
  ;; CEDET                                                              2.7
  ;; "Collection of Emacs Development Environment Tools"
  (defvar section-mode-cedet nil)
  ;; if you want to use emacs included CEDET set to nil
  ;; otherwise set the path of cedet.el and you need to remove:
  ;;    "your-emacs-path/lisp/cedet"
  ;;    "your-emacs-path/lisp/speedbar.*"
  ;;    "your-emacs-path/lisp/emacs-lisp/eieio*"
  (progn
    (defvar profile-cedet-path (concat (file-name-as-directory dotemacs-path) "plugins/cedet-1.1/common/cedet.elc"))
    (defvar profile-gnu-global (concat (file-name-as-directory dotemacs-path) "global"))
    (defvar profile-gnu-global-gtags (concat (file-name-as-directory dotemacs-path) "gtags"))

    ;; SEMANTIC                                                         2.7.1
    ;; FILE: dotemacs/mode-semantic.el
    ;; can do tag, list of function/variable..., preproc, etc
    (defvar section-mode-cedet-semantic nil)
    (progn
      (defvar profile-ede-project nil)
      ) ; (progn
    ;;
    ;; ECB                                                              2.7.2
    ;; FILE: dotemacs/mode-ecb.el
    ;; "Emacs Code Browser"
    ;; can display other windows or speedbar to view folder tree, source
    ;; list, variable/function list, buffer history, etc
    (defvar section-mode-cedet-ecb nil)
    (progn
      (defvar profile-ecb-source-path nil)
      (defvar profile-ecb-excluded-directories-regexps nil)
      (defvar profile-ecb-source-file-regexps nil)
      (defvar profile-ecb-sources-exclude-cvsignore nil)
      (defvar profile-ecb-history-make-buckets nil)
      ) ; (progn
    ) ; (progn

  ;;
  ;; BATCH                                                              2.8
  ;; mode for .bat script in MS Windows
  (defvar section-mode-batch nil)
  ;;
  ;; VISUAL BASIC                                                       2.9
  ;; mode for VisualBasic and VisualBasicAdvance
  (defvar section-mode-vb nil)
  ;;
  ;; WINDOW NUMBERING                                                   2.10
  ;; give a number of each window to easily jump in it
  (defvar section-mode-window-numbering nil)
  ;;
  ;; C                                                                  2.11
  ;; define new type in C
  (defvar section-mode-c nil)
  (progn
    ;; CWARN                                                            2.11.1
    ;; display small error in source code (forget semi-colon, etc)
    (defvar section-mode-c-cwarn nil)
    ;;
    ;; DATA DEBUG                                                       2.11.3
    ;; ??  (not used)
    (defvar section-mode-c-data-debug nil)
    ) ; (progn

  ;;
  ;; ICOMPLETION                                                        2.12
  ;; more completion in Minibuffer
  (defvar section-mode-icompletion nil)

  ;;
  ;; YASNIPPET                                                          2.13
  ;; snippet mode (not used)
  (defvar section-mode-yasnippet nil)

  ;;
  ;; BROWSE KILL RING                                                   2.14
  ;; mode to browse the kill ring memory
  ;; yank only on the first left top window...
  (defvar section-mode-browse-kill-ring nil)

  ;;
  ;; MAGNETI MARELLI                                                    2.15
  (defvar section-mode-mm nil)
  (progn
    ;; EOL                                                              2.15.1
    ;; light syntax color for End Of Line file
    (defvar section-mode-mm-eol nil)
    ;;
    ;; CAN DBC                                                          2.15.3
    ;; light syntax color for Database CAN file
    (defvar section-mode-mm-dbc nil)
    ;;
    ;; CCM DIFF                                                         2.15.3
    ;; light syntax color for synergy diff file
    (defvar section-mode-mm-diff nil)
    ) ; (progn

  ;;
  ;; DIRED+                                                             2.16
  ;; improve Dired mode, color, open with, etc
  (defvar section-mode-dired-plus nil)

  ;;
  ;; GNU/GLOBAL                                                         2.17
  ;; Tag management mode
  ;; use modified gtags.el:
  ;; see function to add from function.el and put the absolute path of
  ;; global executable
  (defvar section-mode-gnu-global nil)
  (progn
    ;; GNU/GLOBAL gtags                                               2.17.1
    (defvar section-mode-gnu-global-gtags nil)
    ;; GNU/GLOBAL ggtags                                              2.17.2
    (defvar section-mode-gnu-global-ggtags nil)
    ) ; (progn
  ;;
  ;; EPROJECT (grischka)                                                2.18
  ;; project management mode (never used)
  (defvar section-mode-eproject nil)
  ;;
  ;; RTRT SCRIPT                                                        2.19
  ;; rtrt script mode (syntax coloration)
  (defvar section-mode-rtrt-script nil)
  ;;
  ;; VC CLEARCASE                                                       2.20
  ;; vc ClearCase mode
  (defvar section-mode-vc-clearcase nil)
  (progn
    (defvar profile-clearcase-vtree "clearvtree")
    (defvar profile-cleartool "cleartool")
    ) ; (progn
  ;;
  ;; CLEARCASE                                                          2.21
  ;; ClearCase mode
  (defvar section-mode-clearcase nil)
  (progn
    (defvar section-mode-clearcase-el nil)
    ) ; (progn
  ;;
  ;; AUTOHOTKEY                                                         2.22
  ;; AutoHotKey mode
  (defvar section-mode-autohotkey nil)
  ;;
  ;; OUTLINE                                                            2.23
  ;; Outline mode to manually hide/show source code block
  (defvar section-mode-outline nil)
  ;;
  ;; AUTO HIGHLIGHT SYMBOL                                              2.24
  ;; to automatically highlight symbol at point
  (defvar section-mode-auto-highlight-symbol nil)
  ;;
  ;; GOOGLE CALENDAR                                                    2.25
  ;; to import Google calendar
  (defvar section-mode-google-calendar nil)
  ;;
  ;; FILL COLUMN INDICATOR                                              2.26
  ;; show a line at fill-column (set at 80 in dotemacs/my-misc.el
  ;; be careful enable truncate line
  (defvar section-mode-fill-column-indicator nil)
  ;;
  ;; MUSE                                                               2.27
  ;; show a line at fill-column (set at 80 in dotemacs/my-misc.el
  ;; muse mode to have nice doc
  (defvar section-mode-muse nil)
  ;;
  ;; UNDO TREE                                                          2.28
  ;; replace the undo built in function
  (defvar section-mode-undo-tree nil)
  ;;
  ;; CSV                                                                2.29
  ;; parse/edit/sort CSV file
  (defvar section-mode-csv nil)
  ;;
  ;; SUBVERSION                                                         2.30
  ;; support Subversion 1.7
  (defvar section-mode-subversion nil)
  ;;
  ;; DIFF COLOR                                                         2.31
  ;; add color to diff mode
  (defvar section-mode-diff-color nil)
  ;;
  ;; DIRED SORT                                                         2.32
  ;; more option to sort in Dired mode
  (defvar section-mode-dired-sort nil)
  ;;
  ;; ORG MODE                                                           2.33
  ;; to organize everything (also use on Android)
  (defvar section-mode-org-mode nil)
  (progn
    (defvar profile-org-directory "")
    (defvar profile-org-agenda-files "")
    (defvar profile-org-default-notes-file "")
    (defvar section-mode-org-default-as-init-buffer nil)
    )
  ;;
  ;; ISEARCH+                                                           2.34
  ;; add some features to isearch
  (defvar section-mode-isearch+ nil)
  ;;
  ;; PSVN                                                               2.35
  ;; add features to subversion integration
  (defvar section-mode-psvn nil)
  ;;
  ;; POWERLINE                                                          2.36
  ;; fancy modeline
  (defvar section-mode-powerline nil)
  ;;
  ;; NYAN                                                               2.37
  ;; add a bar in modeline with position in buffer with the nyan cat
  (defvar section-mode-nyan nil)
  ;;
  ;; SML                                                                2.38
  ;; add a bar in modeline with position in buffer
  (defvar section-mode-sml nil)
  ;;
  ;; DIRED                                                              2.39
  ;; change option to command ls for dired mode
  (defvar section-mode-dired nil)
  ;;
  ;; ISEARCH                                                            2.40
  ;; scroll is possible when incremental search
  (defvar section-mode-isearch nil)
  ;;
  ;; RAINBOW DELIMITERS                                                 2.41
  ;; scroll is possible when incremental search
  (defvar section-mode-rainbow-delimiters nil)
  ;;
  ;; CALFW                                                              2.42
  ;; a more graphical calendar (like google agenda)
  (defvar section-mode-calfw nil)
  ;;
  ;; DIRED DETAILS                                                      2.43
  ;; show hide details in dired mode
  (defvar section-mode-dired-details nil)
  ;;
  ;; SMART TAB                                                          2.44
  ;; expand or indent at the point with tab
  (defvar section-mode-smart-tab nil)
  ;;
  ;; FOLD DWIM                                                          2.45
  ;; show hide code source block
  (defvar section-mode-fold-dwim nil)
  ;;
  ;; DIRED LETTER ISEARCH                                               2.46
  ;; activate by default iserach in dired mode
  (defvar section-mode-dired-lis nil)
  ;;
  ;; NXHTML                                                             2.47
  ;;
  (defvar section-mode-nxhtml nil)
  ;;
  ;; FASTNAV                                                            2.48
  ;; fast navigation like with zap-to-char but only to move
  (defvar section-mode-fastnav nil)
  ;;
  ;; MRU YANK                                                           2.49
  ;; MRU (Most Recently Used) in kill-ring
  (defvar section-mode-mru-yank nil)
  ;;
  ;; ACK                                                                2.50
  ;; search with ack (no more grep)
  (defvar section-mode-ack nil)
  (progn
    (defvar section-mode-ack-full nil)
    (defvar section-mode-ack-and-half nil)
    (defvar section-mode-ack-emacs nil)
    )
  ;;
  ;; ACE JUMP                                                           2.51
  ;; move quickly and easily with ace jump
  ;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
  (defvar section-mode-ace-jump nil)
  (progn
    ;;
    ;;; AVY
    ;; move quickly and easily with avy like with ace jump
    (defvar section-mode-avy nil)
    )
  ;;
  ;; DIREDFUL                                                           2.52
  ;; color dired buffer
  (defvar section-mode-diredful nil)
  ;;
  ;; PS2PDF                                                             2.53
  ;; print buffer/region in pdf
  (defvar section-mode-ps2pdf nil)
  ;;
  ;; AUCTEX                                                             2.54
  ;; latex mode
  (defvar section-mode-auctex nil)
  ;;
  ;; HELM                                                               2.55
  ;; helm mode (fork of anything mode)
  (defvar section-mode-helm nil)
  (progn
    ;; replace bookmark list
    (defvar section-mode-helm-bookmark nil)
    ;; replace electric buffer list
    (defvar section-mode-helm-buffers-list nil)
    ;; replace browse kill ring
    (defvar section-mode-helm-kill-ring nil)
    ;; replace M-x
    (defvar section-mode-helm-M-x nil)
    ;; replace occur
    (defvar section-mode-helm-occur nil)
    ;; replace find files C-x C-f
    (defvar section-mode-helm-find-files nil)
    ;; replace recentf
    (defvar section-mode-helm-recentf nil)
    ;; add imenu bind
    (defvar section-mode-helm-imenu nil)
    ) ; (progn
  ;;
  ;; YASCROLL                                                           2.56
  ;; add a small visual scroll-bar (can not be used with mouse click)
  (defvar section-mode-yascroll nil)
  (progn
    ;; time before hide scroll-bar (nil to always show)
    (defvar profile-yascroll-delay-to-hide nil)
    ) ; (progn
  ;;
  ;; SMART-FORWARD                                                      2.57
  ;; move in code with semantic see example in plugins/smart-forward.el
  (defvar section-mode-smart-forward nil)
  ;;
  ;; RAINBOW MODE                                                       2.58
  ;; show string color in color
  (defvar section-mode-rainbow nil)
  ;;
  ;; EDIFF                                                              2.59
  ;; graphical diff (## to toggle whitespace ignoring)
  (defvar section-mode-ediff nil)
  ;;
  ;; MAGIT                                                              2.60
  ;; use git with nice interface (do not use vc interface from emacs)
  (defvar section-mode-magit nil)
  (progn
    (defvar profile-magit-exec "git")
    )
  ;;
  ;; SYNERGY                                                            2.61
  ;; use synergy without java client GUI (do not use vc interface from emacs)
  (defvar section-mode-synergy nil)
  (progn
    (defvar profile-synergy-username "")
    (defvar profile-synergy-database "")
    (defvar profile-synergy-server "")
    (defvar profile-synergy-history-filter nil)
    (defvar profile-synergy-diff-external-command nil)
    (defvar profile-synergy-diff-external-parameter nil)
    (defvar profile-synergy-diff-external-swap-file nil)
    )
  ;;
  ;; HIDE-LINES                                                         2.62
  ;; hide lines using regexp (like narrow but with regex and not region)
  (defvar section-mode-hide-lines nil)
  ;;
  ;; AGGRESSIVE-INDENT                                                  2.63
  ;; indent all line in function/condition in C or lisp mode when edit it
  (defvar section-mode-aggressive-indent nil)
  ;;
  ;; PLATINIUM SEARCH                                                   2.64
  ;; A front-end for pt, The Platinum Searcher (faster than ack)
  (defvar section-mode-platinium-search nil)
  ;;
  ;; POPWIN                                                             2.65
  ;; A pop-up manager for annoying buffer (have like ECB compilation buffer)
  (defvar section-mode-popwin nil)
  ;;
  ;; PROJECTILE                                                         2.66
  ;; Project management, filtered find-file, only with root file from version
  ;; control
  (defvar section-mode-projectile nil)
  ;;
  ;; COMPANY MODE                                                       2.67
  ;; Completion mode using back-ends to have symbol
  (defvar section-mode-company nil)
  ;;
  ;; EXPAND-REGION                                                      2.68
  ;; Increase selected region by semantic units
  (defvar section-mode-expand-region nil)
  ;;
  ;; FUNCTION-ARGS                                                      2.69
  ;; Show function parameters in C and C++
  (defvar section-mode-function-args nil)
  ;;
  ;; ELPY                                                               2.70
  ;; Python mode like an IDE
  (defvar section-mode-elpy nil)
  ;;
  ;; SMARTPARENS                                                        2.71
  ;; useful to have nice navigation through source code structure
  (defvar section-mode-smartparens nil)
  ;;
  ;; PLANTUML                                                           2.72
  ;; generate uml diagram from text
  (defvar section-mode-plantuml nil)
  ;;
  ;; GRAPHVIZ DOT                                                       2.73
  ;; generate diagram from text
  (defvar section-mode-graphviz-dot nil)
  ;;
  ;; HASKELL                                                            2.74
  ;; editing, debugging and developing Haskell programs
  (defvar section-mode-haskell nil)
  ;;
  ;; CFLOW                                                              2.75
  ;; useful to have call tree in C source code
  (defvar section-mode-cflow nil)
  ;;
  ;; IRONY                                                              2.76
  ;; improving the editing experience for the C, C++ and Objective-C
  ;; using clang (only work after emacs 24.4 under MS Windows)
  ;; need cmake and clang in path and exec path of emacs (profile-path and
  ;; profile-exec-path)
  (defvar section-mode-irony nil)
  ;;
  ;; MARKDOWN                                                           2.77
  ;; mode to edit Markdown-formatted text (by example wiki of github)
  (defvar section-mode-markdown nil)
  (progn
    ;; to enable markdown mode with github flavoured for all .md files and not
    ;; only for README.md
    (defvar section-mode-markdown-github nil)
    )
  ;;
  ;; DIMINISH                                                           2.99
  ;; shrink major and minor mode name in the modeline
  (defvar section-mode-diminish nil)
  ) ; (progn


;;
;;; LANGUAGES                                                           3
;; FILE: dotemacs/languages.el
(defvar section-languages nil)
(progn
  ;; C                                                                  3.1
  ;; set indentation style and preprocessing option
  (defvar section-languages-c nil)
  (progn
    (defvar profile-c-indent-offset 4)
    (defvar profile-c-extra-types nil)
    (defvar profile-c-macro-preprocessor "cpp -C")
    (defvar profile-c-macro-cppflags "")
    (defvar profile-c-ask-before-compile nil)
    ;; INDENT PREPROCESSOR
    (defvar section-languages-c-indent-preprocessor nil)
    ;; HIDE SHOW
    (defvar section-languages-c-hide-show nil)
    (progn
      (defvar section-languages-c-hide-show-hide-all-at-start nil)
      ) ; (progn
    ;; FLYMAKE
    (defvar section-languages-c-flymake nil)
    ;; FLYCHECK (replacement of flymake using irony mode)
    (defvar section-languages-c-flycheck nil)
    ) ; (progn
  ;;
  ;; LISP                                                               3.2
  ;; set indentation style
  (defvar section-languages-lisp nil)
  (progn
    (defvar profile-lisp-indent-offset 4)
    ) ; (progn
  ;;
  ;; TAB                                                                3.3
  ;; tab always in space
  (defvar section-languages-tabulation nil)
  ;;
  ;; RTRT SCRIPT PTU                                                    3.4
  ;; set indentation style
  (defvar section-languages-rtrt-script nil)
  (progn
    (defvar profile-rtrt-indent-offset 4)
    ) ; (progn
  ;;
  ;; PERL                                                               3.5
  ;; set indentation style
  (defvar section-languages-perl nil)
  (progn
    (defvar profile-perl-indent-offset 4)
    ) ; (progn
  ;;
  ;; C++ QT                                                             3.6
  ;; set include for Qt 4.8
  (defvar section-languages-c++-qt nil)
  ) ; (progn


;;
;;; SELECTION                                                           4
;; FILE: dotemacs/selection.el
;; selection can be kill + selection is highlight + kill->copy in read only
(defvar section-selection nil)
(progn
  ;; SHIFT SELECTION                                                    4.1
  ;; selection can be done with shit and arrow keys (default setting since 23.3)
  (defvar section-selection-with-shift nil)
  ) ; (progn


;;
;;; DISPLAY                                                             5
(defvar section-display nil)
(progn
  ;; WINDOWS/BUFFERS                                                    5.1
  ;; FILE: dotemacs/display-buffer.el
  ;; buffers with *buffername* should be displayed in the same window
  ;; first column in window will display buffer limit
  ;; next page will leave 5 shared line
  (defvar section-display-windows-buffers nil)
  (progn
    ;;; VISUAL LINE                                                     5.1.1
    ;; word wrap, truncate line without cut word
    ;; END and HOME will go to the end/start of screen line not logical line
    (defvar section-display-windows-buffers-visual-line nil)
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
  (defvar section-display-font nil)
  (progn
    ;; ANTIALIAS
    ;; set antialiasing on font rendering
    (defvar section-display-font-antialias nil)
    (defvar profile-font nil)
    ;;
    ;; INTERNATIONAL                                                    5.3.2
    ;; ISO or utf-8 or ...  (not used)
    (defvar section-display-font-international nil)
    ) ; (progn

  ;;
  ;; COLOR                                                              5.4
  ;; FILE: dotemacs/display-color.el
  ;; set manual color
  (defvar section-display-color nil)
  (progn
    ;; PARENTHESES MODE                                                 5.4.1
    ;; matched parentheses are highlight
    (defvar section-display-color-parentheses-mode nil)
    ;; PARENTHESES MINIBUFFER                                           5.4.2
    ;; matched parentheses are highlight and if not visible show it in the
    ;; Minibuffer
    (defvar section-display-color-parentheses-visible nil)
    ;; PARENTHESES HIGHLIGHT                                            5.4.3
    ;; matched parentheses are highlight in rainbow color
    (defvar section-display-color-parentheses-highlight nil)

    ;; COLOR THEME                                                      5.4.4
    ;; set color by color-theme mode
    (defvar section-display-color-theme nil)
    (progn
      (defvar profile-color-theme "zenburn")
      ) ; (progn

    ;;
    ;; ANSI COLOR COMPILE WINDOW                                        5.4.5
    ;; have color and no more junk like this ^[[32m
    (defvar section-display-color-ansi-color-compile nil)

    ;;
    ;; HIGHLIGHT CURRENT LINE                                           5.4.6
    ;; have current line highlighted
    (defvar section-display-color-highlight-line nil)
    ) ; (progn
  ) ; (progn


;;
;;; INTERFACE                                                           6
;; FILE: dotemacs/interface.el
;; display buffer name in titlebar (example "<[ foobar.c ]>")
(defvar section-interface nil)
(progn
  ;; DECORATION                                                         6.1
  ;; remove all mouse interface (toolbar, menubar, scrollbar)
  (defvar section-interface-remove-decoration nil)
  ;;
  ;; MODELINE                                                           6.2
  ;; FILE: dotemacs/interface-modeline.el
  ;; set some option to add in the grey line at the bottom of each buffer
  (defvar section-interface-modeline nil)
  ;; WINDOW TITLE
  ;; %b buffername ; %F frame name ; %l line number ; %c column number
  ;; %p percent of buffer above top ; %m mode name ; %n Narrow mode
  ;; %z coding systems ; %Z %z + end-of-line format ; %- infinitely dashes
  (defvar profile-window-title "<[ %b ]>")
  ;; TRANSPARENCY                                                       6.3
  ;; the whole emacs will be transparent
  (defvar section-interface-transparency nil)
  (progn
    (defvar profile-transparency 100)
    ) ; (progn
  ;;
  ;; FULLSCREEN                                                         6.4
  ;; REQUIREMENT:       section-environment-os-recognition
  (defvar section-interface-fullscreen nil)
  ;;
  ;; ECB                                                                6.5
  ;; FILE: dotemacs/interface-ecb.el
  ;; REQUIREMENT:       section-mode-cedet-ecb
  ;; set size, display, refresh and remove opening tips
  (defvar section-interface-ecb nil)
  (progn
    ;; ECB ASCII TREE
    ;; display ascii guides instead of image for arborescence tree      6.4.1
    (defvar section-interface-ecb-ascii-tree nil)
    )
  ) ; (progn


;;
;;; COMPLETION                                                          7
;; FILE: dotemacs/completion.el
;; active letter case completion + dynamic completion
(defvar section-completion nil)


;;
;;; SHORTCUT                                                            8
(defvar section-shortcut nil)
(progn
  ;; GLOBAL                                                             8.1
  ;; FILE: dotemacs/shortcut-global.el
  ;; add global shortcut
  (defvar section-shortcut-global nil)
  (progn
    ;; CUA                                                              8.1.1
    (defvar section-shortcut-global-cua nil)
    ) ; (progn

  ;; WINDOWS                                                            8.2
  ;; FILE: dotemacs/shortcut-windows.el
  ;; add shortcut to manage windows
  (defvar section-shortcut-windows nil)
  ;;
  ;; BUFFERS                                                            8.3
  ;; FILE: dotemacs/shortcut-buffers.el
  ;; add shortcut to manage buffers
  (defvar section-shortcut-buffers nil)

  ;; ECB                                                                8.4
  ;; FILE: dotemacs/shortcut-ecb.el
  ;; REQUIREMENT:       section-mode-cedet-ecb
  ;; add shortcut to manage ecb windows
  (defvar section-shortcut-ecb nil)

  ;; GREP                                                               8.5
  ;; FILE: dotemacs/shortcut-grep.el
  ;; add shortcut to manage grep
  (defvar section-shortcut-grep nil)

  ;; FUNCTION                                                           8.6
  ;; FILE: dotemacs/shortcut-function.el
  ;; add shortcut to manage custom functions
  (defvar section-shortcut-function nil)

  ;; TAGS                                                               8.7
  ;; FILE: dotemacs/shortcut-tags.el
  ;; add shortcut to manage gtags or etags
  (defvar section-shortcut-tags nil)
  (progn
    ;; ETAGS                                                            8.7.1
    (defvar section-shortcut-tags-exuberant-ctags nil)
    ;;
    ;; GTAGS                                                            8.7.2
    ;; REQUIREMENT:       section-mode-gnu-global
    (defvar section-shortcut-tags-gnu-global nil)
    ) ; (progn

  ;; SEMANTIC                                                           8.8
  ;; FILE: dotemacs/shortcut-semantic.el
  ;; REQUIREMENT:       section-mode-cedet-semantic
  ;; add shortcut to move in source code with semantic
  (defvar section-shortcut-semantic nil)
  ) ; (progn


;;
;;; MOUSE                                                               9
;; FILE: dotemacs/my-mouse.el
;; REQUIREMENT: section-environment-terminal-vs-graphics
;; smooth wheel + lazy decoration when scroll
(defvar section-mouse nil)
(progn
  ;;
  ;; PASTE CURSOR                                                       9.1
  ;; yank at point not mouse cursor (either when yank with mouse wheel)
  (defvar section-mouse-paste-to-point-not-mouse-cursor nil)
  ;;
  ;; AVOIDANCE                                                          9.2
  ;; mouse cursor avoid the keyboard cursor when typing
  (defvar section-mouse-avoidance nil)
  ;;
  ;; SMOOTH SCROLL                                                      9.3
  ;; it will always scroll line by line with arrow at start or end of screen
  (defvar section-mouse-smooth-scroll nil)
  ) ; (progn


;;
;;; ANNOYANCES                                                          10
;; FILE: dotemacs/annoyances.el
;; no welcome message + yes->y + do not query to refresh buffer + remove insert
;; key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog box + no
;; tooltips
(defvar section-annoyances nil)
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
  (defvar section-annoyances-backup-file-in-directory nil)
  ) ; (progn


;;
;;; MISC                                                                11
;; FILE: dotemacs/my-misc.el
;; remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
(defvar section-misc nil)
(progn
  ;; SPACE
  (defvar profile-remove-useless-ending-space nil)
  (defvar profile-always-new-line-at-end nil)
  ;;
  (defvar profile-username "noname")
  (defvar profile-latitude 0.00)
  (defvar profile-longitude 0.00)
  (defvar profile-location-name "Neverland")
  (defvar profile-google-calendar-user      "yourmail@gmail.com")
  (defvar profile-google-calendar-src       (concat (file-name-as-directory dotemacs-path) "plugins/google"))
  (defvar profile-google-calendar-directory (concat (file-name-as-directory dotemacs-path) "cache"))
  (defvar profile-google-calendar-url       "http://www.google.com/calendar/ical/yourmail%40gmail.com/private-xxxxxxxxxxxxxxxxxxxxxxxxxxxxx/basic.ics")
  (defvar profile-backup-directory (concat (file-name-as-directory dotemacs-path) "backup"))
  (defvar profile-autosave-directory (concat (file-name-as-directory dotemacs-path) "cache"))
  (defvar profile-fill-column 78)
  (defvar profile-browser "firefox")
  ;;
  ;; CALENDAR                                                           11.1
  ;; set latitude/longitude + location + holidays + custom date in Modeline
  ;; lunar phase, sunrise/sunset, time etc
  (defvar section-misc-calendar nil)
  (progn
    ;; CALENDAR in French
    (setq section-misc-calendar-french nil)
    ) ; (progn
  ;;
  ;; DICTIONARY                                                         11.2
  ;; set default dictionary, etc
  (defvar section-misc-dictionary nil)
  (progn
    (defvar profile-ispell-program "aspell")
    (defvar profile-ispell-dictionary "english")
    ) ; (progn
  ;;
  ;; BOOKMARK                                                           11.3
  ;; set default bookmark storage
  (defvar section-misc-bookmark nil)
  (progn
    (defvar profile-bookmark-sort nil)
    ) ; (progn
  ;;
  ;; SCREENSAVER                                                        11.4
  ;; set screensaver when idle time higher than 5 minutes
  (defvar section-misc-screensaver nil)
  ) ; (progn


;;
;;; CUSTOMIZE                                                           12
;; FILE: dotemacs/my-custom.el
;; all customize settings are put in here when you use interface (customize)
;; to change settings
(defvar section-filecustomize nil)


;;
;;; AFTER LOADING CONF
;; this function will be call at the end after all configuration, it can be use
;; to override some settings or add settings without modify the configuration
(defun function-to-call-after-loading-conf ())


;; add to load path the dotemacs directory
(add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "dotemacs"))

;;
;;; ENVIRONMENT
(when section-environment (message "0 Environment...")
  (try-require 'environment "  ")
  (message "0 Environment... Done"))

;;
;;; FUNCTIONS
(when section-functions (message "1 Functions...")
  (try-require 'functions "  ")
  (message "1 Functions... Done"))

;;
;;; MODE
(when section-mode (message "2 Mode...")
  (try-require 'mode "  ")
  (message "2 Mode... Done"))

;;
;;; LANGUAGES
(when section-languages (message "3 Languages...")
  (try-require 'languages "  ")
  (message "3 Languages... Done"))

;;
;;; SELECTION
(when section-selection (message "4 Selection...")
  (try-require 'selection "  ")
  (message "4 Selection... Done"))

;;
;;; DISPLAY
(when section-display (message "5 Display...")
  (try-require 'display "  ")
  (message "5 Display... Done"))


;;
;;; INTERFACE
(when section-interface (message "6 Interface...")
  (try-require 'interface "  ")
  (message "6 Interface... Done"))

;;
;;; COMPLETION
(when section-completion (message "7 Completion...")
  (try-require 'completion "  ")
  (message "7 Completion... Done"))

;;
;;; SHORTCUT
(when section-shortcut (message "8 Shortcut...")
  (try-require 'shortcut "  ")
  (message "8 Shortcut... Done"))

;;
;;; MOUSE
(when section-mouse (message "9 Mouse...")
  (try-require 'my-mouse "  ")
  (message "9 Mouse... Done"))

;;
;;; ANNOYANCES
(when section-annoyances (message "10 Annoyances...")
  (try-require 'annoyances "  ")
  (message "10 Annoyances... Done"))

;;
;;; MISC
(when section-misc (message "11 Misc...")
  (try-require 'my-misc "  ")
  (message "11 Misc... Done"))

;;
;;; CUSTOMIZE
(when section-filecustomize (message "12 File custom...")
  ;; customize modification (made by Emacs interface) are put in custom.el
  (setq custom-file (concat (file-name-as-directory dotemacs-path) "dotemacs/my-custom.el"))
  (try-require 'my-custom "  ")
  (message "12 File custom... Done"))

;;
;;; AFTER LOADING CONF
(function-to-call-after-loading-conf)

;;
;;; WARNINGS
;; show warning buffer if exist
(when (get-buffer "*Warnings*")
  (switch-to-buffer "*Warnings*"))

;;; emacs.el ends here
