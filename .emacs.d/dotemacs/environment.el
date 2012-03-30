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
;; Version: 1.5
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment'

;;; Change Log:
;; 2012-03-28 (1.5)
;;    translate comments in english
;; 2012-03-02 (1.4)
;;    add working environment and change "OS" message
;; 2012-03-02 (1.3)
;;    add/change/remove some path
;; 2011-07-09 (1.2)
;;    add running-graphics or terminal
;; 2011-04-21 (1.1)
;;    add running-on-ms-windows or gnu-linux
;; 2010-10-11 (1.0)
;;    add add path for ms windows
;; 2010-06-11 (0.1)
;;    creation from scratch + env test from other .emacs


;;; Code:
;;
;;; OS RECOGNITION
(when section-environment-os-recognition (message "  0.1 OS Recognition...")
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
  (message "  0.1 OS Recognition... Done"))

;;
;;; WORKING ENVIRONMENT
;; only displayed message (settings are done in ../emacs.el)
(when section-environment-working-message (message "  0.2 Working Environment...")
  (cond
    ;; Magneti Marelli ---------------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (message "    * Magneti Marelli")
      ) ; Magneti Marelli

    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (message "    * Alstom Transport")
      ) ; Alstom Transport

    ) ; cond -------------------------------------------------------------------
  (message "  0.2 Working Environment:... Done"))

;;
;;; CYGWIN
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-cygwin (message "  0.3 Cygwin...")
  (if running-on-ms-windows
    ;; to integrate cygwin with emacs (mostly for grep-find)
    (progn
      (cond
        ;; Magneti Marelli -----------------------------------------------------
        ((string= clt-working-environment "Magneti Marelli")
          (defvar cygwin-bin "d:/cygwin/bin")
          (defvar gnu-bin "d:/cygwin/usr/bin/gnuwin32/bin")
          (defvar cv-bin "C:/Program Files/IBM/RationalSDLC/ClearCase/bin")
          (defvar win-path "")
          ) ; Magneti Marelli

        ;; Alstom Transport ----------------------------------------------------
        ((string= clt-working-environment"Alstom Transport")
          (defvar cygwin-bin "d:/cygwin/bin")
          (defvar cv-bin "C:/Program Files/IBM/RationalSDLC/ClearCase/bin")
          (defvar gnu-bin "d:/Users/ctete/tools/gnuwin32/bin")
          ;; I put the whole PATH Environment variable to work with clearcase
          (defvar win-path "d:/cygwin/bin;c:/WINDOWS;c:/WINDOWS/System32;d:/cygwin/bin;c:/WINDOWS;c:/WINDOWS/System32;/usr/local/bin;/usr/bin;/bin;c:/Program Files/IBM/RationalSDLC/common;c:/Program Files/PRQA/PDFReports/texmf/miktex/bin;c:/Program Files/Analog Devices/VisualDSP;c:/Program Files/Analog Devices/VisualDSP/System;c:/WINDOWS/system32;c:/WINDOWS;c:/WINDOWS/System32/Wbem;c:/Program Files/QuickTime/QTSystem;c:/Program Files/Fichiers communs/Aladdin Shared/eToken/PKIClient/x32;d:/system/Notes;c:/Program Files/Symantec/pcAnywhere;%Program Files%/UltraEdit;c:/Program Files/IBM/RationalSDLC/ClearCase/etc/utils;c:/Program Files/Rational/TestRealTime/bin/intel/win32;c:/Program Files/Rational/common;c:/Program Files/Lotus/Notes;c:/Program Files/IBM/RationalSDLC/ClearCase/bin;d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/site/bin;d:/Users/ctete/tools/strawberry-perl-5.14.2.1/perl/bin;d:/Users/ctete/tools/strawberry-perl-5.14.2.1/c/bin;d:/Users/ctete/tools/gnuwin32/bin;C:/Python27")
          ) ; Alstom Transport

        ) ; cond ---------------------------------------------------------------
      (setenv "PATH"
        (concat win-path ";" cygwin-bin ";" gnu-bin ";" cv-bin ";"))
      (setq exec-path
        '(win-path cygwin-bin gnu-bin cv-bin))
      )
    )
  (message "  0.3 Cygwin... Done"))

;;
;;; TERMINAL VS GRAPHICS
(when section-environment-terminal-vs-graphics (message "  0.4 Terminal VS Graphics...")
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
  (message "  0.4 Terminal VS Graphics... Done"))

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
        (custom-set-variables
          '(ediff-cmp-program "d:/cygwin/bin/cmp.exe")
          '(ediff-diff-program "d:/cgwin/bin/diff.exe")
          '(ediff-diff3-program "d:/cygwin/bin/diff3.exe")
          '(shell-file-name "D:/cygwin/bin/bash.exe")
          ;;'(shell-file-name "C:/WINDOWS/system32/cmd.exe")
          )
        (setq explicit-bash-args '("--login" "-i"))
        ) ; Alstom Transport
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

  ;;;; set package server
  ;;; only with Emacs 24
  (setq package-archives
    '(("ELPA" . "http://tromey.com/elpa/")
       ("gnu" . "http://elpa.gnu.org/packages/")
       ("marmalade" . "http://marmalade-repo.org/packages/")
       )
    )
  (message "  0.7 ELPA... Done"))

;;; environment.el ends here
