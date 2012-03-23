;;; mode.el --- a config file for all mode settings

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

;; Keywords: config, mode
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.4
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-mode'

;;; Change Log:
;; 2012-03-18 (1.3)
;;    add mode for RTRT script (.ptu)
;; 2012-03-03 (1.2)
;;    add selection of wich cedet is used
;; 2011-04-21 (1.1)
;;    add some test about ms windows for paths
;; 2010-10-13 (1.0)
;;    split for ecb parameter
;; 2006-10-13 (0.1)
;;    creation from scratch + doxymacs (no history since)


;;; Code:
;; Chemin du repertoires customize (si jamais ce n'est pas fait au dessus)
(add-to-list 'load-path  dotemacs-path)
(add-to-list 'load-path  (concat dotemacs-path "/plugins"))
(setq load-path (cons (expand-file-name dotemacs-path) load-path))
(setq load-path (cons (expand-file-name (concat dotemacs-path "/plugins")) load-path))

;;
;;; DOXYMACS
;; need to configure and use it
;; REQUIREMENT: var     `section-mode-doxymacs'
(when section-mode-doxymacs (message "  3.1 Doxymacs...")
  (when (try-require 'doxymacs)
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    (defvar doxymacs-doxygen-style "JavaDoc")
    (defun my-doxymacs-font-lock-hook ()
      (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
    (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook))
  (message "  3.1 Doxymacs... Done"))

;;
;;; IDO
;; REQUIREMENT: var     `section-mode-ido'
;; meilleur 'switch buffers' (C-x C-b ou M-a) et 'open file' (C-x C-f)
;; pose un petit souci avec les fichiers ayant des noms un peu exotique
;; il apparait une erreur avec le module 'tramp'
(when section-mode-ido (message "  3.2 Ido...")
  (when (try-require 'ido)
    (ido-mode t))
  (message "  3.2 Ido... Done"))

;;
;;; UNIQUIFY
;; REQUIREMENT: var     `section-mode-uniquify'
;; create unique buffer names with shared directoy components)
(when section-mode-uniquify (message "  3.3 Uniquify...")
  (when (try-require 'uniquify)
    (custom-set-variables
      '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))))
  (message "  3.3 Uniquify... Done"))

;;
;;; CEDET
;; REQUIREMENT: var     `section-mode-cedet'
;;              var     `clt-cedet-path'
(when section-mode-cedet
;; to remove warning:
;; Warning: cedet-called-interactively-p called with 0 arguments, but requires 1
  (setq byte-compile-warnings nil)
  ;; if the path is define use it to load cedet
  (if clt-cedet-path
    (progn
      (message "  3.4 CEDET bzr...")
      ;; REQUIREMENT:
      ;; need to remove `your-emacs-path/lisp/cedet'
      ;;                `your-emacs-path/lisp/speedbar.*'
      ;;                `your-emacs-path/lisp/emacs-lisp/eieio*'
      (defvar clt-cedet-bzr t)
      )
    (progn
      (message "  3.4 emacs included CEDET...")
      (defvar clt-cedet-bzr nil)
      )
    )
  ;; init the state of the loading of cedet
  (defvar clt-cedet-loaded nil)
  (if clt-cedet-bzr
    ;; load from the path clt-cedet-path
    (progn
      (if (load-file clt-cedet-path)
        (setq clt-cedet-loaded t)
        (message "    cedet was not loaded. Have you removed your-emacs-path/lisp/cedet/ your-emacs-path/lisp/speedbar.* and your-emacs-path/lisp/emacs-lisp/eieio* ?")
        )
      )
    ;; load from the emacs included cedet
    (progn
      (if (try-require 'cedet)
        (setq clt-cedet-loaded t)
        (message "    cedet was not loaded. Have you GNU/Emacs 23.x ?")
        )
      )
    )
  ;; only if cedet can be loaded
  (when clt-cedet-loaded
    ;;
    ;;; SEMANTIC
    ;; REQUIREMENT:     var     `section-mode-cedet-semantic'
    (when section-mode-cedet-semantic (message "    3.4.1 Semantic...")
      (load-file (concat dotemacs-path "/dotemacs/mode-semantic.el"))
      (message "    3.4.1 Semantic... Done"))

    ;;
    ;;; ECB (Emacs Code Browser)
    ;; REQUIREMENT:     var     `section-mode-cedet-ecb'
    (when section-mode-cedet-ecb (message "    3.4.2 ECB...")
      (load-file (concat dotemacs-path "/dotemacs/mode-ecb.el"))
      (message "    3.4.2 ECB... Done"))
    )
  (message "  3.4 CEDET... Done"))

;;
;;;    BATCH
;; REQUIREMENT: var     `section-mode-batch'
;; coloration syntaxique des scripts .bat de windows
(when section-mode-batch (message "  3.5 Batch Windows...")
  (autoload 'batch-mode "batch-mode" "Load batch-mode")
  (add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
  (message "  3.5 Batch Windows... Done"))

;;
;;; VISUAL BASIC
;; REQUIREMENT: var     `section-mode-vb'
;; coloration syntaxique des sources en vb et vba
(when section-mode-vb (message "  3.6 Visual Basic...")
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                    visual-basic-mode)) auto-mode-alist))
  (message "  3.6 Visual Basic... Done"))

;;
;;; WINDOW NUMBERING
;; REQUIREMENT: var     `section-mode-window-numbering'
;; raccourci numerotes vers les windows
;; (donne un numero au windows et permet de switcher facilement avec le
;; raccourci `M-<numero>').
(when section-mode-window-numbering (message "  3.7 Windows Numbering...")
  (when (try-require 'window-numbering)
    (window-numbering-mode 1))
  (message "  3.7 Windows Numbering... Done"))

;;
;;; C
;; REQUIREMENT: var     `section-mode-c'
(when section-mode-c (message "  3.8 C...")
  ;; Nouveaux types
  (defvar c-font-lock-extra-types
    (list "ubyte" "ushort" "ulong" "ulonglong" "sbyte" "sshort" "slong" "slonglong"))

  ;;;; TRY
  ;;;; define extra C types to font-lock
  ;;(setq c-font-lock-extra-types
  ;;  (append
  ;;    '("CHAR" "BOOL" "BYTE" "SOCKET" "boolean" "UINT" "UINT16"
  ;;       "UINT32" "ULONG" "FLOAT" "INT" "INT16" "INT32""uint" "ulong" "string"
  ;;       "BOOLEAN" "\\sw+_T")
  ;;    c-font-lock-extra-types))

  ;;; CWARN
  ;; REQUIREMENT:       var     `section-mode-c-cwarn'
  ;; affiche les petits warning de code
  ;; (ex: affectation dans test, point virgule apres condition...)
  (when section-mode-c-cwarn (message "    3.8.1 CWarn...")
    (cwarn-mode t)
    (message "    3.8.1 CWarn... Done"))

  ;;; DATA DEBUG
  ;; REQUIREMENT:       var     `section-mode-c-data-debug'
  ;; affiche les petits warning de code
  ;; voir raccourci M-:
  (when section-mode-c-data-debug (message "2.8.2 C Data Debug...")
    (when (try-require 'data-debug))
    (message "2.8.2 C Data Debug... Done"))

  (autoload 'rebox-comment "rebox" nil t)
  (autoload 'rebox-region "rebox" nil t)
  (setq rebox-default-style 245)

  ;;; Compilation mode
  (setq compile-command "ccm objectmake")
  (message "  3.8 C... Done"))

;;
;;; ICOMPLETION
;; REQUIREMENT: var     `section-mode-icompletion'
(when section-mode-icompletion (message "  3.9 Icompletion...")
  ;; Completion pour la barre 'minibuffer'
  (icomplete-mode t)
  (message "  3.9 Icompletion... Done"))

;;
;;; YASNIPPET
;; REQUIREMENT: var     `section-mode-yasnippet'
(when section-mode-yasnippet (message "  3.10 Yasnippet...")
;; Completion pour le c
  (add-to-list 'load-path (concat dotemacs-path "/plugins/yasnippet-0.6.1c"))
  (when (try-require 'yasnippet) ; not yasnippet-bundle
    (setq yas/root-directory (concat dotemacs-path "/plugins/yasnippet-0.6.1c/snippets"))
    (yas/load-directory yas/root-directory))
  (yas/global-mode)
  (message "  3.10 Yasnippet... Done"))

;;
;;; BROWSE KILL RING
;; REQUIREMENT: var     `section-mode-browse-kill-ring'
(when section-mode-browse-kill-ring (message "  3.11 Browse Kill-Ring...")
  (try-require 'browse-kill-ring)
  (browse-kill-ring-default-keybindings)
  (message "  3.11 Browse Kill-Ring... Done"))

;;
;;; MAGNETI MARELLI
;; REQUIREMENT: var     `section-mode-mm'
(when section-mode-mm (message "  3.12 Magneti Marelli...")
  ;;; EOL
  ;; REQUIREMENT:       var     `section-mode-mm-eol'
  (when section-mode-mm-eol (message "    3.12.1 EOL...")
    (load-file (concat dotemacs-path "/plugins/mm-eol.el"))
    (message "    3.12.1 EOL... Done"))

  ;;; CAN DBC
  ;; REQUIREMENT:       var     `section-mode-mm-dbc'
  (when section-mode-mm-dbc (message "    3.12.2 CAN Dbc...")
    (load-file (concat dotemacs-path "/plugins/mm-dbc.el"))
    (message "    3.12.2 CAN Dbc... Done"))

  ;;; CCM DIFF
  ;; REQUIREMENT:       var     `section-mode-mm-diff'
  (when section-mode-mm-diff (message "    3.12.3 Synergy Diff...")
    (load-file (concat dotemacs-path "/plugins/mm-diff.el"))
    (message "    3.12.3 Synergy Diff... Done"))
  (message "  3.12 Magneti Marelli... Done"))

;;
;;; DIRED+
;; REQUIREMENT: var     `section-mode-dired-plus'
(when section-mode-dired-plus (message "  3.13 Dired+...")
  (try-require 'dired+)
  (message "  3.13 Dired+... Done"))

;;
;;; GNU/GLOBAL
;; REQUIREMENT: var     `section-mode-gnu-global'
(when section-mode-gnu-global (message "  3.14 GNU/Global...")
  (try-require 'gtags)
  (autoload 'gtags-mode "gtags" "" t)
  (defun gtags-c-mode ()
    (gtags-mode 1)
    (setq gtags-select-buffer-single t)
    )
  (add-hook 'c-mode-common-hook 'gtags-c-mode)
  (message "  3.14 GNU/Global... Done"))

;;
;;; EPROJECT (grischka) ; never used
;; REQUIREMENT: var     `section-mode-eproject'
(when section-mode-eproject (message "  3.15 Eproject...")
  (load-file (concat dotemacs-path "/plugins/eproject.el"))
  ;;(when (try-require 'eproject))
  (message "  3.15 Eproject... Done"))

;;
;;; RTRT SCRIPT
;; REQUIREMENT: var     `section-mode-rtrt-script'
(when section-mode-rtrt-script (message "  3.16 RTRT script...")
  (load-file (concat dotemacs-path "/plugins/rtrt-script.elc"))
  (message "  3.16 RTRT script... Done"))

;;
;;; VC CLEARCASE
;; REQUIREMENT: var     `section-mode-vc-clearcase'
(when section-mode-vc-clearcase (message "  3.17 VC ClearCase...")
  (add-to-list 'load-path  (concat dotemacs-path "/plugins/vc-clearcase-3.6"))
  (setq load-path (append load-path '(concat dotemacs-path "/plugins/vc-clearcase-3.6")))
  (load "vc-clearcase-auto")
  (custom-set-variables
    '(clearcase-checkout-comment-type (quote normal))
    ;;'(clearcase-mode-line-function nil)
    '(clearcase-use-external-diff t)
    '(clearcase-vtree-program "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/clearvtree.exe")
    '(cleartool-program "C:/Program Files/IBM/RationalSDLC/ClearCase/bin/cleartool.exe")
    )
  (message "  3.17 VC ClearCase... Done"))

;;
;;; CLEARCASE
;; REQUIREMENT: var     `section-mode-clearcase'
(when section-mode-clearcase (message "  3.18 ClearCase...")
  (load "clearcase")
  (message "  3.18 ClearCase... Done"))

;;
;;; AUTOHOTKEY
;; REQUIREMENT: var     `section-mode-autohotkey'
(when section-mode-autohotkey (message "  3.19 AutoHotKey...")
  ;; this folder doesn't exist anymore ; os the mode doesn't work
  (setq ahk-syntax-directory "PATHTO/AutoHotkey/Extras/Editors/Syntax/")
  (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
  (autoload 'ahk-mode-fix "ahk-mode")
  (message "  3.19 AutoHotKey... Done"))

(custom-set-variables

;;
;;; HIDE IFDEF
  '(hide-ifdef-initially t)
  '(hide-ifdef-lines nil)
  '(hide-ifdef-shadow t)
  '(hs-hide-comments-when-hiding-all nil)
  )

;; TRY
;; need to try
;(autoload 'ifdef:ifdef-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:ifndef-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:ifdef-else-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:if-region "ifdef" "ifdef your code" t)
;(autoload 'ifdef:if-else-region "ifdef" "ifdef your code" t)

;;; mode.el ends here
