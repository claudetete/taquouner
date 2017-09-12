;;; 03-languages.el --- a config file for programing languages

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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 3.7
;; Created: October 2006
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [HEADER.Set style and/or indentation for multiple languages]
;; [DEFAULT.t]

;;; Change Log:
;; 2017-09-11 (3.7)
;;    fix C setting error + add arduino specific settings
;; 2017-07-25 (3.6)
;;    update to new conf format
;; 2017-05-26 (3.5)
;;    add flycheck hook on C mode + run perl file directly when instance
;;    compile mode
;; 2016-09-28 (3.4)
;;    replace about electric-pair to add new emacs version 25
;; 2013-05-23 (3.3)
;;    add condition to set semantic option for c++
;; 2013-05-07 (3.2)
;;    fix indent options from profile
;; 2012-12-27 (3.1)
;;    flymake not by default in c++ mode
;; 2012-12-04 (3.0)
;;    compile command confirmation defined by profile
;; 2012-11-29 (2.9)
;;    add flymake mode
;; 2012-10-26 (2.8)
;;    add outline minor mode for C
;; 2012-10-18 (2.7)
;;    add config for C++ with Qt
;; 2012-07-18 (2.6)
;;    add hide show mode + use hook with lisp
;; 2012-07-11 (2.5)
;;    fix bug with emacs 23.x with electric pair
;; 2012-07-09 (2.4)
;;    electric pair for parenthesis, bracket, quote, etc
;; 2012-06-13 (2.3)
;;    remove unknown option
;; 2012-06-12 (2.2)
;;    add hook for Hide/Show mode in C mode
;; 2012-06-08 (2.1)
;;    redo remove compile without asking + add section to indent macro
;; 2012-06-05 (2.0)
;;    remove all profile dependances + use profile values + remove never used c
;;    style
;; 2012-05-29 (1.9)
;;    to remove asking about command ine to compile
;; 2012-04-20 (1.8)
;;    add working environment default
;; 2012-04-06 (1.7)
;;    add Perl indent
;; 2012-03-29 (1.6)
;;    translate comments in English + add rtrt script indent
;; 2012-03-02 (1.5)
;;    add working environment condition
;; 2011-10-21 (1.4)
;;    add some style for C (no really used)
;; 2011-08-02 (1.3)
;;    add customization of c-macro variables
;; 2011-07-09 (1.2)
;;    rename from indentation to languages
;; 2011-04-21 (1.1)
;;    add hook for c-mode and indentation of case
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch + tab = x space (no history since)


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

    ;; [VARCOMMENT.HIDE SHOW: use outline minor mode to fold source code block]
    ;; [VARIABLE.tqnr-section-languages-c-hide-show t]
    (when tqnr-section-languages-c-hide-show
      (outline-minor-mode t)
      ;; [VARCOMMENT.HIDE ALL AT START: hide all when opening file]
      ;; [VARIABLE.tqnr-section-languages-c-hide-show-hide-all-at-start nil]
      (when tqnr-section-languages-c-hide-show-hide-all-at-start
        (hide-sublevels 1))
      )
    )

  (when tqnr-section-languages-c-hide-show
    (add-hook 'outline-minor-mode-hook
      (lambda ()
        ;; hide all
        (local-set-key  (kbd "C-,")     (lambda ()
                                          (interactive)
                                          (hide-sublevels 1)))
        ;; show all
        (local-set-key  (kbd "C-.")     'show-all)
        )
      )
    ) ;; (when tqnr-section-languages-c-hide-show

  ;; set c mode
  (add-hook 'c-mode-common-hook 'tqnr-c-mode)

  ;; start Hide/Show mode
  ;; use with TAB or S-MiddleClick
  (add-hook 'c-mode-common-hook 'hs-minor-mode)

  ;; [VARCOMMENT.FLYMAKE: verification error/warning in source code on the fly]
  ;; [VARIABLE.tqnr-section-languages-c-flymake nil]
  (when tqnr-section-languages-c-flymake
    ;; auto syntax check
    (try-require 'flymake "    ")
    (try-require 'flymake-clang-c "    ")
    (add-hook 'c-mode-hook 'flymake-clang-c-load)
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


(provide '03-languages)

;;; 03-languages.el ends here
