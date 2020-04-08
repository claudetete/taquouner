;;; 03-languages.el --- a config file for programing languages -*- lexical-binding: t -*-

;; Copyright (c) 2006-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
;; Version: 3.8
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [HEADER.Set style and/or indentation for multiple languages]
;; [DEFAULT.t]


;;; Code:
;;
;; [[SUBCOMMENT.
;; C
;;   language settings, set indentation style and preprocessing option
;; ]]
;; [SUBSECTION.tqnr-section-languages-c t]
(when tqnr-section-languages-c (message "   Languages C...")

  ;; must be only load with c-mode else some variables are not defined
  (defun tqnr-c-mode ()
    ;; style k&r and not gnu
    (c-set-style "k&r")
    ;;(c-set-style "eZSystems")

    ;; [VARCOMMENT.number of space for indentation in C]
    ;; [VARIABLE.tqnr-profile-c-indent-offset 2]
    (setq c-basic-offset tqnr-profile-c-indent-offset)
    ;; increase "case" indentation in a "switch"
    (c-set-offset 'case-label '+)
    ;; hungry mode is enable, for hungry delete...
    (c-toggle-hungry-state t)

    ;; [VARCOMMENT.new types (add name string in list)]
    ;; [[VARIABLE.tqnr-profile-c-extra-types
    ;;   '(
    ;;      "ubyte"
    ;;      "ushort"
    ;;      "ulong"
    ;;      "ulonglong"
    ;;      "sbyte"
    ;;      "sshort"
    ;;      "slong"
    ;;      "slonglong"
    ;;      )
    ;; ]]
    (setq c-font-lock-extra-types (append tqnr-profile-c-extra-types c-font-lock-extra-types))

    ;; [VARCOMMENT.Compile mode without ask]
    ;; [VARIABLE.tqnr-profile-c-ask-before-compile t]
    (setq compilation-read-command tqnr-profile-c-ask-before-compile)

    (when (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
      ;; pair of parenthesis, bracket, etc
      (electric-pair-mode t))

    ;; hide show minor mode
    (hs-minor-mode)

    ;; [VARCOMMENT.INDENT PREPROCESSOR: make a #define be align with C code]
    ;; [VARIABLE.tqnr-section-languages-c-indent-preprocessor nil]
    (when tqnr-section-languages-c-indent-preprocessor
      (c-set-offset 'cpp-macro 0))
    )

  ;; set c mode
  (add-hook 'c-mode-common-hook 'tqnr-c-mode)

  ;; start Hide/Show mode
  ;; use with TAB or S-MiddleClick
  (add-hook 'c-mode-common-hook 'hs-minor-mode)

  ;; [VARCOMMENT.FLYMAKE: verification error/warning in source code on the fly]
  ;; [VARIABLE.tqnr-section-languages-c-flymake nil]
  (when tqnr-section-languages-c-flymake
    ;; auto syntax check
    (use-package flymake)
    ;; use flymake-cppcheck instead
    ;;(try-require 'flymake-clang-c "      ")
    ;;(add-hook 'c-mode-hook 'flymake-clang-c-load)
    )

  ;; [VARCOMMENT.FLYCHECK: verification error/warning in source code on the fly]
  ;; [VARIABLE.tqnr-section-languages-c-flycheck nil]
  (when tqnr-section-languages-c-flycheck
    ;; auto syntax check
    (add-hook 'c-mode-hook 'flycheck-mode))


  (custom-set-variables
    ;; [VARCOMMENT.command to preprocess]
    ;; [VARIABLE.tqnr-profile-c-macro-preprocessor "cpp -C"]
    '(c-macro-preprocessor tqnr-profile-c-macro-preprocessor)
    ;; do not prompt for flags
    '(c-macro-prompt-flag nil)
    ;; [VARCOMMENT.set flags about macro preprocessing]
    ;; [VARIABLE.tqnr-profile-c-macro-cppflags "-D__DEBUG__"]
    '(c-macro-cppflags tqnr-profile-c-macro-cppflags)
    ;; resize the height of the window like the size of the expand macro
    '(c-macro-shrink-window-flag t)
    )

  (message "   Languages C... Done"))

;; [[SUBCOMMENT.
;; LISP
;;   set indentation style
;; ]]
;; [SUBSECTION.tqnr-section-languages-lisp t]
(when tqnr-section-languages-lisp (message "    Languages Lisp...")
  (add-hook 'lisp-interaction-mode-hook
    '(lambda ()
       ;; [VARCOMMENT.number of space for indentation in lisp]
       ;; [VARIABLE.tqnr-profile-lisp-indent-offset 2]
       (setq lisp-indent-offset tqnr-profile-lisp-indent-offset)
       (when (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
         ;; pair of parenthesis, bracket, etc
         (electric-pair-mode t))

       ;; hide show minor mode
       (hs-minor-mode)
       )
    )
  (message "    Languages Lisp... Done"))

;; [[SUBCOMMENT.
;; TABULATION
;;   tab always in space
;; ]]
;; [SUBSECTION.tqnr-section-languages-tabulation t]
(when tqnr-section-languages-tabulation (message "    Tabulation...")
  ;; set number of space for One indentation
  (setq-default indent-tabs-mode nil)
  ;;
  ;;;; modify size of tab (M-i) /* do not do what I want */
  ;;;; /* try with 'edit-tab-stops'
  ;;(setq-default tab-width 2)
  (message "    Tabulation... Done"))

;; [[SUBCOMMENT.
;; RTRT SCRIPT PTU
;;   set indentation style
;; ]]
;; [SUBSECTION.tqnr-section-languages-rtrt-script nil]
(when tqnr-section-languages-rtrt-script (message "    RTRT script ptu...")
  (custom-set-variables
    ;; [VARCOMMENT.set number of space for indentation in rtrt script .ptu]
    ;; [VARIABLE.tqnr-profile-rtrt-indent-offset 2]
    '(rtrt-script-indent tqnr-profile-rtrt-indent-offset)
   )
  (message "    RTRT script ptu... Done"))

;; [[SUBCOMMENT.
;; PERL
;;   set indentation style
;; ]]
;; [SUBSECTION.tqnr-section-languages-perl t]
(when tqnr-section-languages-perl (message "    Languages Perl...")
  ;; [VARCOMMENT.number of space for indentation in perl]
  ;; [VARIABLE.tqnr-profile-perl-indent-offset 2]
  (setq perl-indent-level tqnr-profile-perl-indent-offset)
  ;; add customize compile command line to execute current python file
  ;; found at http://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile
  (add-hook 'perl-mode-hook
    (lambda ()
      (set (make-local-variable 'compile-command)
        (concat "perl " (shell-quote-argument buffer-file-name)))))
  (message "    Languages Perl... Done"))

;; [[SUBCOMMENT.
;; C++ QT
;;   set include for Qt 4.8
;; ]]
;; [SUBSECTION.tqnr-section-languages-c++-qt nil]
(when tqnr-section-languages-c++-qt (message "    Languages C++ Qt...")
  (when (and tqnr-section-mode-cedet (and tqnr-section-mode-cedet-bzr tqnr-section-mode-cedet-loaded))
    (setq qt4-base-dir "C:/QtSDK/Desktop/Qt/4.8.1/mingw/include")
    (setq qt4-gui-dir (concat qt4-base-dir "/QtGui"))
    (setq qt4-core-dir (concat qt4-base-dir "/QtCore"))
    (setq qt4-network-dir (concat qt4-base-dir "/QtNetwork"))
    (semantic-add-system-include qt4-base-dir 'c++-mode)
    (semantic-add-system-include qt4-gui-dir 'c++-mode)
    (semantic-add-system-include qt4-core-dir 'c++-mode)
    (semantic-add-system-include qt4-network-dir 'c++-mode)
    (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
    ) ; (when (and tqnr-section-mode-cedet (and tqnr-section-mode-cedet-bzr tqnr-section-mode-cedet-loaded))

  (message "    Laguages C++ Qt... Done"))

;; [[SUBCOMMENT.
;; ARDUINO
;;   set indentation style
;; ]]
;; [SUBSECTION.tqnr-section-languages-arduino nil]
(when tqnr-section-languages-arduino (message "    Languages Arduino...")
  ;; must be only load arduino-mode which is loaded after c mode
  (defun tqnr-arduino-mode ()
    ;; [VARCOMMENT.number of space for indentation in Arduino]
    ;; [VARIABLE.tqnr-profile-arduino-indent-offset 2]
    (setq c-basic-offset tqnr-profile-arduino-indent-offset)
    )
  ;; set c mode
  (add-hook 'arduino-mode-hook 'tqnr-arduino-mode)
  (message "    Languages Arduino... Done"))

(setq sh-basic-offset 2)

(provide '03-languages)

;;; 03-languages.el ends here
