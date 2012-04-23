;;; environment.el --- a config file for environment settings

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

;; Keywords: config, environment, os, path
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.7
;; Created: October 2006
;; Last-Updated: April 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment'

;;; Change Log:
;; 2012-04-20 (1.7)
;;    add default environment
;; 2012-04-03 (1.6)
;;    add terminal environment to Alstom Transport
;; 2012-03-28 (1.5)
;;    translate comments in english
;; 2012-03-02 (1.4)
;;    add working environment and change "OS" message
;; 2012-03-02 (1.3)
;;    add/change/remove some path
;; 2011-07-09 (1.2)
;;    add running-in-graphical or terminal
;; 2011-04-21 (1.1)
;;    add running-on-ms-windows or gnu-linux
;; 2010-10-11 (1.0)
;;    add add path for ms windows
;; 2010-06-11 (0.1)
;;    creation from scratch + env test from other .emacs


;;; Code:
;;
;;; WORKING ENVIRONMENT
;; can overwrite settings done in ../emacs.el
(when section-environment-working-message (message "  0.1 Working Environment...")
  (cond
    ;; Magneti Marelli ---------------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (message "    * Magneti Marelli")
      ) ; Magneti Marelli

    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (if (window-system)
        (progn
          (defvar clt-cedet-path (concat dotemacs-path "/plugins/cedet-snap/common/cedet.elc"))
          (message "    * Alstom Transport")
          )
        (progn
          ;; can overwrite some option from ../emacs.el
          (setq section-environment-elpa nil)
          (setq section-display-color nil)
          ;;(setq section-display-color-theme t)
          (setq section-display-color-mode nil)
          (setq section-display-color-grep nil)
          (setq section-display-color-ecb nil)
          (setq section-mode-cedet nil)
          (setq section-mode-vc-clearcase nil)
          (setq section-mode-clearcase nil)
          (setq section-mode-google-calendar nil)
          (setq section-display-windows-buffers-transparency nil)
          (setq section-display-ecb nil)
          (setq section-display-color-ecb nil)
          (setq section-shortcut-ecb nil)
          (setq section-shortcut-semantic nil)
          (setq section-mouse nil)
          (setq section-misc-calendar nil)
          (message "    * Alstom Transport Cygwin")
          )
        ) ; if
      ) ; Alstom Transport

    ;; default -----------------------------------------------------------------
    ((string= clt-working-environment "default")
      (defvar clt-cedet-path (concat dotemacs-path "/plugins/cedet-1.1/common/cedet.elc"))

      ;; ENVIRONMENT
      (setq section-environment                        t) ; if I am here it is already true
      (setq section-environment-os-recognition         t)
      (setq section-environment-terminal-vs-graphics   t)
      (setq section-environment-working-message        t)
      (setq section-environment-cygwin                 t)
      (setq section-environment-ms-windows-performance t)
      (setq section-environment-executable             t)
      (setq section-environment-elpa                   nil)

      ;; INTERFACE
      (setq section-interface            t)
      (setq section-interface-decoration nil)
      (setq section-interface-fullscreen nil)
      (setq section-interface-modeline   t)

      ;; EXTERN FILES
      (setq section-external           t)
      (setq section-external-directory t)
      (setq section-external-functions t)
      (setq section-external-vectra    nil)
      (setq section-external-setnu     nil)
      (setq section-external-home-end  t)

      ;; MODE
      (setq section-mode                       t)
      (setq section-mode-doxymacs              nil)
      (setq section-mode-ido                   nil)
      (setq section-mode-uniquify              t)
      (setq section-mode-cedet                 t)
      (setq section-mode-cedet-semantic        t)
      (setq section-mode-cedet-ecb             t)
      (setq section-mode-batch                 t)
      (setq section-mode-vb                    t)
      (setq section-mode-window-numbering      nil)
      (setq section-mode-c                     t)
      (setq section-mode-c-cwarn               nil)
      (setq section-mode-c-data-debug          nil)
      (setq section-mode-icompletion           nil)
      (setq section-mode-yasnippet             t)
      (setq section-mode-browse-kill-ring      nil)
      (setq section-mode-mm                    nil)
      (setq section-mode-mm-eol                nil)
      (setq section-mode-mm-dbc                nil)
      (setq section-mode-mm-diff               nil)
      (setq section-mode-dired-plus            t)
      (setq section-mode-gnu-global            t)
      (setq section-mode-eproject              nil)
      (setq section-mode-rtrt-script           nil)
      (setq section-mode-vc-clearcase          nil)
      (setq section-mode-clearcase             nil)
      (setq section-mode-autohotkey            nil)
      (setq section-mode-outline               t)
      (setq section-mode-auto-highlight-symbol t)
      (setq section-mode-google-calendar       nil)
      (setq section-mode-fill-column-indicator nil)
      (setq section-mode-muse                  nil)

      ;; LANGUAGES
      (setq section-languages             t)
      (setq section-languages-c           t)
      (setq section-languages-lisp        t)
      (setq section-languages-tabulation  t)
      (setq section-languages-rtrt-script nil)
      (setq section-languages-perl        t)

      ;; SELECTION
      (setq section-selection            t)
      (setq section-selection-with-shift t)

      ;; DISPLAY
      (setq section-mydisplay                            t)
      (setq section-display-windows-buffers              t)
      (setq section-display-windows-buffers-transparency nil)
      (setq section-display-speedbar                     nil)
      (setq section-display-ecb                          t)
      (setq section-display-font                         t)
      (setq section-display-font-international           nil)
      (setq section-display-color                        t)
      (setq section-display-color-theme                  t)
      (setq section-display-color-misc                   nil)
      (setq section-display-color-parentheses-mode       nil)
      (setq section-display-color-parentheses-visible    nil)
      (setq section-display-color-parentheses-highlight  nil)
      (setq section-display-color-mode                   nil)
      (setq section-display-color-grep                   nil)
      (setq section-display-color-ecb                    nil)

      ;; COMPLETION
      (setq section-completion t)

      ;; SHORTCUT
      (setq section-shortcut                       t)
      (setq section-shortcut-global                t)
      (setq section-shortcut-global-cua            nil) ; you can use it but I don't recommand it
      (setq section-shortcut-windows               t)
      (setq section-shortcut-buffers               t)
      (setq section-shortcut-ecb                   t)
      (setq section-shortcut-grep                  t)
      (setq section-shortcut-function              t)
      (setq section-shortcut-tags                  t)
      (setq section-shortcut-tags-exhuberant-ctags nil)
      (setq section-shortcut-tags-gnu-global       t)
      (setq section-shortcut-semantic              t)

      ;; MOUSE
      (setq section-mouse                                 t)
      (setq section-mouse-paste-to-point-not-mouse-cursor nil)

      ;; ANNOYANCES
      (setq section-annoyances                                 t)
      (setq section-annoyances-truncate-line                   t)
      (setq section-annoyances-scroll-preserve-cursor-position t)
      (setq section-annoyances-no-backup-file                  nil)
      (setq section-annoyances-backup-file-in-directory        t)

      ;; MISC
      (setq section-misc            t)
      (setq section-misc-calendar   nil)
      (setq section-misc-dictionary nil)

      ;; CUSTOMIZE
      (setq section-filecustomize t)

      (message "    * default")
      ) ; default

    ) ; cond -------------------------------------------------------------------
  (message "  0.1 Working Environment:... Done"))

;;
;;; OS RECOGNITION
(when section-environment-os-recognition (message "  0.2 OS Recognition...")
  (if (string-equal system-type "windows-nt")
    ;; OS - Microsoft Windows
    (progn
      (defvar running-on-ms-windows t)
      (defvar running-on-gnu-linux nil)
      (message "    * Running on MS-Window")
      )
    ;; OS - GNU / Linux
    (progn
      (defvar running-on-ms-windows nil)
      (defvar running-on-gnu-linux t)
      (message "    * Running on GNU/Linux")
      )
    )
  (message "  0.2 OS Recognition... Done"))

;;
;;; TERMINAL VS GRAPHICS
(when section-environment-terminal-vs-graphics (message "  0.3 Terminal VS Graphics...")
  ;; or (display-graphic-p) ? it works like this in MS Windows
  (if (window-system)
    (progn
      (defvar running-in-graphical t)
      (defvar running-in-terminal nil)
      )
    (progn
      (defvar running-in-graphical nil)
      (defvar running-in-terminal t)
      )
    )
  (message "  0.3 Terminal VS Graphics... Done"))

;;
;;; CYGWIN
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-cygwin (message "  0.4 Cygwin...")
  (when running-on-ms-windows
    ;; to integrate cygwin with emacs (mostly for grep-find)
    (cond
      ;; Magneti Marelli -------------------------------------------------------
      ((string= clt-working-environment "Magneti Marelli")
        (defvar cygwin-bin "d:/cygwin/bin")
        (defvar gnu-bin "d:/cygwin/usr/bin/gnuwin32/bin")
        (defvar cv-bin "C:/Program Files/IBM/RationalSDLC/ClearCase/bin")
        (defvar win-path "")
        ) ; Magneti Marelli

      ;; Alstom Transport ------------------------------------------------------
      ((string= clt-working-environment "Alstom Transport")
        (if running-in-graphical
          (progn
            (defvar cygwin-bin "d:/cygwin/bin")
            (defvar cv-bin "C:/Program Files/IBM/RationalSDLC/ClearCase/bin")
            (defvar gnu-bin "d:/Users/ctete/tools/gnuwin32/bin")
            ;; I put the whole PATH Environment variable to work with clearcase
            (defvar win-path "D:/Users/ctete/tools/MikTex/miktex/bin;d:/cygwin/bin;c:/WINDOWS;c:/WINDOWS/System32;d:/cygwin/bin;c:/WINDOWS;c:/WINDOWS/System32;/usr/local/bin;/usr/bin;/bin;c:/Program Files/IBM/RationalSDLC/common;c:/Program Files/PRQA/PDFReports/texmf/miktex/bin;c:/Program Files/Analog Devices/VisualDSP;c:/Program Files/Analog Devices/VisualDSP/System;c:/WINDOWS/system32;c:/WINDOWS;c:/WINDOWS/System32/Wbem;c:/Program Files/QuickTime/QTSystem;c:/Program Files/Fichiers communs/Aladdin Shared/eToken/PKIClient/x32;d:/system/Notes;c:/Program Files/Symantec/pcAnywhere;%Program Files%/UltraEdit;c:/Program Files/IBM/RationalSDLC/ClearCase/etc/utils;c:/Program Files/Rational/TestRealTime/bin/intel/win32;c:/Program Files/Rational/common;c:/Program Files/Lotus/Notes;c:/Program Files/IBM/RationalSDLC/ClearCase/bin;d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/site/bin;d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/bin;d:/Users/ctete/tools/strawberry-perl-5.14.2.1/c/bin;d:/Users/ctete/tools/gnuwin32/bin;C:/Python27")
            )
          (progn
            (defvar cygwin-bin "")
            (defvar cv-bin "/cygdrive/c/Program Files/IBM/RationalSDLC/ClearCase/bin")
            (defvar gnu-bin "")
            ;; I put the whole PATH Environment variable to work with clearcase
            (defvar win-path "/cygdrive/d/cygwin/bin;/cygdrive/c/WINDOWS;/cygdrive/c/WINDOWS/System32;/cygdrive/d/cygwin/bin;/cygdrive/c/WINDOWS;/cygdrive/c/WINDOWS/System32;/usr/local/bin;/usr/bin;/bin;/cygdrive/c/Program Files/IBM/RationalSDLC/common;/cygdrive/c/Program Files/PRQA/PDFReports/texmf/miktex/bin;/cygdrive/c/Program Files/Analog Devices/VisualDSP;/cygdrive/c/Program Files/Analog Devices/VisualDSP/System;/cygdrive/c/WINDOWS/system32;/cygdrive/c/WINDOWS;/cygdrive/c/WINDOWS/System32/Wbem;/cygdrive/c/Program Files/QuickTime/QTSystem;/cygdrive/c/Program Files/Fichiers communs/Aladdin Shared/eToken/PKIClient/x32;/cygdrive/d/system/Notes;/cygdrive/c/Program Files/Symantec/pcAnywhere;%Program Files%/UltraEdit;/cygdrive/c/Program Files/IBM/RationalSDLC/ClearCase/etc/utils;/cygdrive/c/Program Files/Rational/TestRealTime/bin/intel/win32;/cygdrive/c/Program Files/Rational/common;/cygdrive/c/Program Files/Lotus/Notes;/cygdrive/c/Program Files/IBM/RationalSDLC/ClearCase/bin;/cygdrive/d/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/site/bin;/cygdrive/d/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/bin;/cygdrive/d/Users/ctete/tools/strawberry-perl-5.14.2.1/c/bin;/cygdrive/d/Users/ctete/tools/gnuwin32/bin;/CYGDRIVE/C/Python27")
            )
          )
        ) ; Alstom Transport

      ;; default ---------------------------------------------------------------
      ((string= clt-working-environment "default")
        (if running-on-ms-windows
          (if running-in-graphical
            (progn
              ;;FIXME working environment default
              (defvar cygwin-bin "c:/path/to/cywin/bin")
              (defvar cv-bin "c:/path/to/version/control/bin")
              (defvar gnu-bin "c:/path/to/gnuwin32/bin")
              (defvar win-path (getenv "PATH"))
            )
            (progn
              (defvar cygwin-bin (concat "/usr/local/bin" ";" "/usr/bin" ";" "/bin"))
              (defvar cv-bin "/cygdrive/c/path/to/version/control/bin")
              (defvar gnu-bin "/cygdrive/c/path/to/gnuwin32/bin")
              (defvar win-path "/cygdrive/c/all/PATH/value")
              )
            )
          )
        ) ; default

      ) ; cond -----------------------------------------------------------------

    (setenv "PATH"
      (concat win-path ";" cygwin-bin ";" gnu-bin ";" cv-bin ";"))
    (setq exec-path
      '(win-path cygwin-bin gnu-bin cv-bin))
    )
  (message "  0.4 Cygwin... Done"))

;;
;;; MS WINDOWS PERFORMANCE
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-ms-windows-performance (message "  0.5 MS Windows: improve performance...")
  ;; This sets garbage collection to hundred times of the default.  Supposedly
  ;; significantly speeds up startup time. (Seems to work for me, but my
  ;; computer is pretty modern. Disable if you are on anything less than 1
  ;; ghz).
  (setq gc-cons-threshold 50000000)
  ;;
  ;; try to improve slow performance on windows.
  (if running-on-ms-windows
    (setq w32-get-true-file-attributes nil))
  (message "  0.5 Windows: improve performance... Done"))

;;
;;; EXECUTABLE
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-executable (message "  0.6 Executable...")
  (when running-on-ms-windows
    (cond
      ;; Magneti Marelli -------------------------------------------------------
      ((string= clt-working-environment "Magneti Marelli")
        (custom-set-variables
          '(ediff-cmp-program "d:/cygwin/bin/cmp.exe")
          '(ediff-diff-program "d:/cgwin/bin/diff.exe")
          '(ediff-diff3-program "d:/cygwin/bin/diff3.exe")
          '(shell-file-name "D:/cygwin/bin/bash.exe"))
        ) ; Magneti Marelli

      ;; Alstom Transport ------------------------------------------------------
      ((string= clt-working-environment"Alstom Transport")
        (when running-in-graphical
          (custom-set-variables
            '(ediff-cmp-program "d:/cygwin/bin/cmp.exe")
            '(ediff-diff-program "d:/cgwin/bin/diff.exe")
            '(ediff-diff3-program "d:/cygwin/bin/diff3.exe")
            '(shell-file-name "D:/cygwin/bin/bash.exe")
            ;;'(shell-file-name "C:/WINDOWS/system32/cmd.exe")
            ))
        (setq explicit-bash-args '("--login" "-i"))
        ) ; Alstom Transport

      ;; default ---------------------------------------------------------------
      ((string= clt-working-environment"default")
        (when running-in-graphical
          (custom-set-variables
            '(ediff-cmp-program "c:/path/to/cygwin/bin/cmp.exe")
            '(ediff-diff-program "c:/path/to/cgwin/bin/diff.exe")
            '(ediff-diff3-program "c:/path/to/cygwin/bin/diff3.exe")
            ;;FIXME working environment default
            '(shell-file-name "c:/path/to/cygwin/bin/bash.exe")
            ))
        (setq explicit-bash-args '("--login" "-i"))
        ) ; default

      ) ; cond -----------------------------------------------------------------
    )
  (message "  0.6 Executable... Done"))

;;
;;; ELPA
(when section-environment-elpa (message "  0.7 ELPA...")
  ;; This was installed by package-install.el.  This provides support for the
  ;; package system and interfacing with ELPA, the package archive.  Move this
  ;; code earlier if you want to reference packages in your .emacs.
  (when (load (expand-file-name (concat dotemacs-path "/plugins/elpa/package.el"))) (package-initialize))

  ;; set path where put all packages
  (setq package-user-dir (concat dotemacs-path "/plugins/elpa"))

  ;;;; set package server
  ;;; only with Emacs 24 ?
  (setq package-archives
    '(
       ("ELPA"      . "http://tromey.com/elpa/")
       ("gnu"       . "http://elpa.gnu.org/packages/")
       ("marmalade" . "http://marmalade-repo.org/packages/")
       ("melpa"     . "http://melpa.milkbox.net/packages/")
       )
    )
  (message "  0.7 ELPA... Done"))

;;; environment.el ends here