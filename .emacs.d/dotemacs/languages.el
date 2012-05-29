;;; languages.el --- a config file for programing languages

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

;; Keywords: config, languages, lisp, c, tabulation
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-languages'

;;; Change Log:
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
  ;; create a C style from Jan Borsodi
  (defconst ezsystems-c-style
    ;; Always indent c/c++ sources, never insert tabs
    '((c-tab-always-indent        . t)
       (c-basic-offset . 4)
       ;; Offset for line only comments
       (c-comment-only-line-offset . 0)
       ;; Controls the insertion of newlines before and after braces.
       (c-hanging-braces-alist     . ((substatement-open after)
                                       (brace-list-open)))
       ;; Controls the insertion of newlines before and after certain
       ;; colons.
       (c-hanging-colons-alist     . ((member-init-intro before)
                                       (inher-intro)
                                       (case-label after)
                                       (label after)
                                       (access-label after)))
       ;; List of various C/C++/ObjC constructs to "clean up".
       (c-cleanup-list             . (scope-operator
                                       empty-defun-braces
                                       defun-close-semi))
       ;; Association list of syntactic element symbols and indentation
       ;; offsets.
       (c-offsets-alist            . (
                                       (arglist-close . c-lineup-arglist)
                                       (substatement-open . 0)
                                       (case-label        . +)
                                       (block-open        . 0)
                                       (access-label      . -)
                                       (label	      . 0)
                                       (knr-argdecl-intro . -)))
                                        ;(c-echo-syntactic-information-p . t)
       )
    "eZ systems Programming Style")

  ;; add personal style
  (c-add-style "eZSystems" ezsystems-c-style)

  ;; must be only load with c-mode else some variables are not defined
  (cond
    ;; Magneti Marelli ---------------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (defun clt-c-mode ()
        ;; style k&r and not gnu
        (c-set-style "k&r")
        ;;(c-set-style "eZSystems")
        ;; set number of space for One indentation
        (setq c-basic-offset 2)
        ;; increase "case" indentation in a "switch"
        (c-set-offset 'case-label '+)
        ;; hungry mode is enable, for hungry delete...
        (c-toggle-hungry-state t)
        )
      ) ; Magneti Marelli

    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (defun clt-c-mode ()
        ;; style k&r and not gnu
        (c-set-style "k&r")
        ;;(c-set-style "eZSystems")
        ;; set number of space for One indentation
        (setq c-basic-offset 3)
        ;; increase "case" indentation in a "switch"
        (c-set-offset 'case-label '+)
        ;; hungry mode is enable, for hungry delete...
        (c-toggle-hungry-state t)
        )

      ;;; Compile mode without ask
      (setq compilation-read-command nil)
      ) ; Alstom Transport

    ;; default -----------------------------------------------------------------
    ((string= clt-working-environment "default")
      (defun clt-c-mode ()
        ;; style k&r and not gnu
        (c-set-style "k&r")
        ;;(c-set-style "eZSystems")
        ;; set number of space for One indentation
        (setq c-basic-offset 2)
        ;; increase "case" indentation in a "switch"
        (c-set-offset 'case-label '+)
        ;; hungry mode is enable, for hungry delete...
        (c-toggle-hungry-state t)
        )
      ) ; default

    ) ; cond -------------------------------------------------------------------

  ;; set c mode
  (add-hook 'c-mode-common-hook 'clt-c-mode)

  (custom-set-variables
    ;; command to preprocess
    '(c-macro-preprocessor "cpp -C")
    ;; do not prompt for flags
    '(c-macro-prompt-flag nil)
    ;; set flags
    '(c-macro-cppflags "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__")
    ;; resize the height of the window like the size of the expand macro
    '(c-macro-shrink-window-flag t)

    ;; ??
    '(c-syntactic-indentation t)
    )

  (message "  3.1 Languages C... Done"))

;;
;;; LISP
(when section-languages-lisp (message "  3.2 Languages Lisp...")
  ;; set indent size
  (setq lisp-indent-offset 2)
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

;;; languages.el ends here
