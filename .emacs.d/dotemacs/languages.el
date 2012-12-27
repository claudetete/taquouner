;;; languages.el --- a config file for programing languages

;; Copyright (c) 2006-2012 Claude Tete
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

;; Keywords: config, languages, lisp, c, tabulation
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 3.1
;; Created: October 2006
;; Last-Updated: December 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-languages'

;;; Change Log:
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
;;; C
;; REQUIREMENT: var     `section-languages-c'
(when section-languages-c (message "  3.1 Languages C...")
  ;; must be only load with c-mode else some variables are not defined
  (defun clt-c-mode ()
    ;; style k&r and not gnu
    (c-set-style "k&r")
    ;;(c-set-style "eZSystems")
    ;; set number of space for One indentation
    (setq c-basic-offset profile-c-indent-offset)
    ;; increase "case" indentation in a "switch"
    (c-set-offset 'case-label '+)
    ;; hungry mode is enable, for hungry delete...
    (c-toggle-hungry-state t)
    ;; new types
    (setq c-font-lock-extra-types (append profile-c-extra-types c-font-lock-extra-types))
    ;; Compile mode without ask
    (setq compilation-read-command profile-c-ask-before-compile)

    (when (and section-environment-version-recognition running-on-emacs-24)
      ;; pair of parenthesis, bracket, etc
      (electric-pair-mode t))

    ;; hide show minor mode
    (hs-minor-mode)

    ;;; INDENT PREPROCESSOR
    ;; make a #define be align with C code
    (when section-languages-c-indent-preprocessor
      (c-set-offset 'cpp-macro 0))

    ;;; HIDE SHOW
    (when section-languages-c-hide-show
      (outline-minor-mode t)
      ;;; HIDE ALL AT START
      ;; hide all when opening file
      (when section-languages-c-hide-show-hide-all-at-start
        (hide-sublevels 1))
      )
    )

  ;; set c mode
  (add-hook 'c-mode-common-hook 'clt-c-mode)

  ;; start Hide/Show mode
  ;; use with TAB or S-MiddleClick
  (add-hook 'c-mode-common-hook 'hs-minor-mode)

  ;;; FLYMAKE
  (when section-languages-c-flymake
    ;; auto syntax check
    (try-require 'flymake "    ")
    (try-require 'flymake-clang-c "    ")
    (add-hook 'c-mode-hook 'flymake-clang-c-load)
    )

  (custom-set-variables
    ;; command to preprocess
    '(c-macro-preprocessor profile-c-macro-preprocessor)
    ;; do not prompt for flags
    '(c-macro-prompt-flag nil)
    ;; set flags
    '(c-macro-cppflags profile-c-macro-cppflags)
    ;; resize the height of the window like the size of the expand macro
    '(c-macro-shrink-window-flag t)
    )

  (message "  3.1 Languages C... Done"))

;;
;;; LISP
(when section-languages-lisp (message "  3.2 Languages Lisp...")
  (add-hook 'lisp-interaction-mode-hook
    '(lambda ()
       ;; set indent size
       (setq lisp-indent-offset profile-lisp-indent-offset)
       (when (and section-environment-version-recognition running-on-emacs-24)
         ;; pair of parenthesis, bracket, etc
         (electric-pair-mode t))

       ;; hide show minor mode
       (hs-minor-mode)
       )
    )
  (message "  3.2 Languages Lisp... Done"))

;;
;;; TABULATION
(when section-languages-tabulation (message "  3.3 Tabulation...")
  ;; set number of space for One indentation
  (setq-default indent-tabs-mode nil)
  ;;
  ;;;; modify size of tab (M-i) /* do not do what I want */
  ;;;; /* try with 'edit-tab-stops'
  ;;(setq-default tab-width 2)
  (message "  3.3 Tabulation... Done"))

;;
;;; RTRT SCRIPT PTU
(when section-languages-rtrt-script (message "  3.4 RTRT script ptu...")
  (custom-set-variables
    ;; set number of space for indentation in rtrt script .ptu
    '(rtrt-script-indent 4)
   )
  (message "  3.4 RTRT script ptu... Done"))

;;
;;; PERL
(when section-languages-perl (message "  3.5 Languages Perl...")
  ;; set indent size
  (setq perl-indent-level 2)
  (message "  3.5 Languages Perl... Done"))


;;
;;; C++ QT
(when section-languages-c++-qt (message "  3.6 Languages C++ Qt...")
  (when (and section-mode-cedet-bzr section-mode-cedet-loaded)
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
    ) ; (when (and section-mode-cedet-bzr section-mode-cedet-loaded)

  (message "  3.6 Laguages C++ Qt...Done"))
(provide 'languages)

;;; languages.el ends here
