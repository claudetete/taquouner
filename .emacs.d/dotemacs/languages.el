;;; languages.el --- a config file for programation languages

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
;; Version: 1.4
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-languages'

;;; Change Log:
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
(when section-languages-c (message "  4.1 Languages C...")
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

  ;; a appeler qu'au chargement du c-mode sinon quelques variables ne sont
  ;; peut etre pas definies
  (cond
    ;; Magneti Marelli ---------------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (defun clt-c-mode ()
        ;; Indentation style k&r and not gnu
        (c-set-style "k&r")
        ;;(c-set-style "eZSystems")
        ;; configuration du nombre d'espace pour une indentation
        (setq c-basic-offset 2)
        ;; change l'indentation du case dans un switch
        (c-set-offset 'case-label '+)
        (c-toggle-hungry-state t)
        )
      ) ; Magneti Marelli

    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (defun clt-c-mode ()
        ;; Indentation style k&r and not gnu
        (c-set-style "k&r")
        ;;(c-set-style "eZSystems")
        ;; configuration du nombre d'espace pour une indentation
        (setq c-basic-offset 3)
        ;; change l'indentation du case dans un switch
        (c-set-offset 'case-label '+)
        (c-toggle-hungry-state t)
        )
      ) ; Alstom Transport

    ) ; cond -------------------------------------------------------------------
  (add-hook 'c-mode-common-hook 'clt-c-mode)

  ;; need to test
  ;; Author: Stefan Reichoer, stefan@xsteve.at
  (when (require 'align nil t)
    (setq xsteve-c-align-rules-list
      `((c-comment-one-line
          (regexp . "[^- \t]\\(\\s-*\\)/\\*.*\\*/$")
          (group  . 1)
          (repeat . nil))

         (c-macro-definition
           (regexp   . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)"))

         (c-macro-line-continuation
           (regexp   . "\\(\\s-*\\)\\\\$")
           (column   . c-backslash-column))

         (c-variable-declaration
           (regexp   . ,(concat "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)"
                          "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
                          "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
                          "\\s-*[;,]\\|)\\s-*$\\)"))
           (group    . 1)
           (justify  . t)
           (valid
             . ,(function
                  (lambda ()
                    (not (or (save-excursion
                               (goto-char (match-beginning 1))
                               (backward-word 1)
                               (looking-at
                                 "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
                           (if (and (boundp 'font-lock-mode) font-lock-mode)
                             (eq (cadr (memq 'face (text-properties-at (point))))
                               'font-lock-comment-face)
                             (eq (caar (c-guess-basic-syntax)) 'c))))))))

         (c-assignment
           (regexp   . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
                          "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
           (group    . (1 2))
           (justify  . t)
           (tab-stop . nil))

         (c-chain-logic
           (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
           (modes    . align-c++-modes)
           (valid    . ,(function
                          (lambda ()
                            (save-excursion
                              (goto-char (match-end 2))
                              (looking-at "\\s-*\\(/[*/]\\|$\\)"))))))
         ))

    (add-hook 'c-mode-hook (lambda () (setq align-mode-rules-list xsteve-c-align-rules-list))))

  (custom-set-variables
    ;; customiser les variables de c-macro (cmacexp.el)
    ;; command to preprocess
    '(c-macro-preprocessor "cpp -C")
    ;; do not prompt for flags
    '(c-macro-prompt-flag nil)
    ;; set flags
    '(c-macro-cppflags "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__")
    ;; resize the height of the window like the size of the epxand macro
    '(c-macro-shrink-window-flag t)

    ;; ??
    '(c-syntactic-indentation t)
    )

  (message "  4.1 Languages C... Done"))

;;
;;; LISP
(when section-languages-lisp (message "  4.2 Languages Lisp...")
  ;; configuration du nombre d'espace pour une indentation
  (setq lisp-indent-offset 2)
  (message "  4.2 Languages Lisp... Done"))

;;
;;; TABULATION
(when section-languages-tabulation (message "  4.3 Tabulation...")
  ;; Change l'indentation et les tabulation (M-i) en espaces
  (setq-default indent-tabs-mode nil)
  ;;
  ;; change la taille des tabulation (M-i) /* ne fait pas ce que je pensais */
  ;; /* lancer 'edit-tab-stops' et changer la taille
  ;;(setq-default tab-width 2)
  (message "  4.3 Tabulation... Done"))

;;; languages.el ends here
