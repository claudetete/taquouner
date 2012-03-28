;;; function.el --- a config file to add some function

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

;; Keywords: config, function
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 3.2
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-external-functions'

;;; Change Log:
;; 2012-03-26 (3.3)
;;    fix bug with rtrt align
;; 2012-03-20 (3.2)
;;    add function for rtrt align and replace
;; 2012-03-12 (3.1)
;;    add conditions for each working environment
;; 2012-03-02 (3.0)
;;    add function occur-at-point
;; 2011-11-03 (2.9)
;;    add function to expand C macro per project
;; 2011-10-27 (2.8)
;;    change grep function per project to add list file
;; 2011-03-10 (2.7)
;;    add gtags function
;; 2010-12-02 (2.6)
;;    add dos2unix and unix2dos
;; 2010-11-19 (2.5)
;;    align regexp
;; 2010-11-03 (2.4)
;;    push line + search fault and tab
;; 2010-09-02 (2.3)
;;    insert tab + config
;; 2010-08-11 (2.2)
;;    select word + startup
;; 2010-07-09 (2.1)
;;    ecb
;; 2010-06-11 (2.0)
;;    grep
;; 2010-06-09 (1.6)
;;    etags
;; 2008-04-21 (1.5)
;;    insert header
;; 2008-03-10 (1.0)
;;    add printf for debug /* Kaneton :) */
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CONST                                                                     ;;
;;  {                                                                         ;;
      (defconst clt-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
        "Regexp matching tag name.")

      (cond
;;      Magneti Marelli --------------------------------------------------------
        ((string= clt-working-environment "Magneti Marelli")
          (defconst clt-nsf-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -DC_COMP_COSMIC_MC9S12 -D__MC9S12xx__ -D__MC9S12XHZ__ -I d:/ccm_wa/NSF/NSF/NSF_CLIENT/out/include")

          (defconst clt-nbnfhl-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/NBNF/NBNF_HL/NBNF_CLIENT_HL/out/include")

          (defconst clt-nbnfll-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -DC_COMP_COSMIC_MC9S12 -D__MC9S12xx__ -D__MC9S12XHZ__ -I d:/ccm_wa/NBNF/NBNF_LL/NSFNBNF_CLIENT/out/include")

          (defconst clt-ecar-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/ECAR/ENSF_CLIENT/out/include")

          (defconst clt-xl1-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/XL1/XL1_CLIENT/out/include")
          ) ;; Magneti Marelli
        ) ;; cond --------------------------------------------------------------
;;  } /* CONST */                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;
;; FLAG DEBUG ;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              printf-debug-shortcut                                 ;;
;;  @CREATED BY:        Kevin Prigent                                         ;;
;;  @ABOUT:             insere un printf pour du debug                        ;;
;;                              printf("\n"); //DEBUG                         ;;
;;  {                                                                         ;;
(defun printf-debug-shortcut ()
  "Insert a printf(\"n\"); //DEBUG."
  (interactive)
  (insert "printf(\"\\n\");//DEBUG")
  (indent-according-to-mode) (backward-char 12)
  )
;;  } /* printf-debug-shortcut */                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ifdef-debug-shortcut                                  ;;
;;  @CREATED BY:        Kevin Prigent                                         ;;
;;  @ABOUT:             insere un test de macro pour du debug                 ;;
;;                              #ifdef OUR_DEBUG                              ;;
;;                              \n                                            ;;
;;                              #endif                                        ;;
;;  {                                                                         ;;
(defun ifdef-debug-shortcut ()
  "Insert a #ifdef OUR_DEBUG."
  (interactive)
  (insert "#ifdef OUR_DEBUG\n"
      "\t\n"
      "#endif\n")
  (indent-according-to-mode) (backward-char 8)
  )
;;  } /* ifdef-debug-shortcut */                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;
;; SELECT WORD ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              clt-match-string                                      ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             match une string                                      ;;
;;  {                                                                         ;;
(defun clt-match-string (n)
  "Match a string (N)."
  (buffer-substring (match-beginning n) (match-end n)))
;;  } /* clt-match-string */                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              clt-select-word                                       ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             selectionne le mot sous le curseur                    ;;
;;  {                                                                         ;;
(defun clt-select-word ()
  "Select the word under cursor."
  (cond
    ((looking-at "[0-9A-Za-z_]")
      (while (and (not (bolp)) (looking-at "[0-9A-Za-z_]"))
        (forward-char -1))
      (if (not (looking-at "[0-9A-Za-z_]")) (forward-char 1)))
    (t
      (while (looking-at "[ \t]")
        (forward-char 1))))
  (if (looking-at clt-symbol-regexp)
    (clt-match-string 0) nil)
  )
;;  } /* clt-select-word */                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              select-word-under                                     ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             selectionne le mot sous le curseur                    ;;
;;  {                                                                         ;;
(defun select-word-under ()
  "Select the word under cursor."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)
    )
  )
;;  } /* select-word-under */                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              occur-word-at-point                                   ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             search the word at the point for the whole buffer     ;;
;;  {                                                                         ;;
(defun occur-word-at-point ()
  "Search the word under cursor in the current buffer."
  (interactive)
  (let (word prompt input)
    (setq word (clt-select-word))
    (if word
      (setq prompt (concat "List lines matching regexp (default " word "): "))
      (setq prompt "List lines matching regexp: "))

    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))

    (occur word)
    )
  )
;;  } /* occur-word-at-point */                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              isearch-forward-at-point                              ;;
;;  @CREATED BY:        from www.emacswiki.org                                ;;
;;  @ABOUT:             incremental search the word at the point              ;;
;;  {                                                                         ;;
 ;; I-search with initial contents
(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  "Set initialization of isearch."
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update)
)
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point (optional REGEXP-P and NO-RECURSIVE-EDIT)."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
            (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
        (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)
        )
      )
    )
  )
;;  } /* isearch-forward-at-point */                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              search-word-at-point                                  ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             incremental search the word at the point              ;;
;;                      do not work :(                                        ;;
;;  {                                                                         ;;
(defun search-word-at-point ()
  "Incremental search the word under cursor."
  (interactive)
  (let (word)
    (setq word (clt-select-word))
    (if word
      (progn
;        (setq
;          isearch-regexp t
;          isearch-string (concat "" word)
;          isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
;          isearch-yank-flag t
;          )
;        )
;      (progn
        ;; For consistent behavior, restart Isearch from starting point
        ;; (or end point if using `isearch-backward') of symbol.
;        (isearch-search)
        (setq isearch-string (concat "" word))
        )
      (message "No symbol at point")
      )
    (isearch-search-and-update)
    )
  )
;;  } /* search-word-at-point */                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; GREP-FIND ;;
;;;;;;;;;;;;;;;

(cond
;; Magneti Marelli -------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              nsf-grep-find                                         ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             grep perso                                            ;;
;;  {                                                                         ;;
(defun nsf-grep-find ()
  "Search word (egrep regex) in all files for NSF project."
  (interactive)
  (let (word prompt input)
    (setq word (clt-select-word))
    (if word
      (setq prompt (concat "Grep word NSF project: (default " word ") "))
      (setq prompt "Grep word (NSF): "))

    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))

    (grep (concat "cat d:/ccm_wa/NSF/NSF/NSF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\"")))

;  (grep "cat d:/ccm_wa/NSF/NSF_CLIENT/NSF.files | xargs grep --color -nsIE lcdc | sed \"s#D:/ccm_wa/NSF/.*\\([^/]+:\\)#\\1#\"")
;  (grep (concat "cat d:/ccm_wa/NSF/GTAGS.files | xargs grep --color -nsIE \""s"\""))
)
;;  } /* nsf-grep-find */                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              nll-grep-find                                         ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             grep perso                                            ;;
;;  {                                                                         ;;
(defun nll-grep-find ()
  "Search word (egrep regex) in all files for NBNF LL project."
  (interactive)
  (let (word prompt input)
    (setq word (clt-select-word))
    (if word
      (setq prompt (concat "Grep word NBNF_LL project: (default " word ") "))
      (setq prompt "Grep word (NBNF_LL): "))

    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))

  (grep (concat "cat d:/ccm_wa/NBNF/NBNF_LL/NSFNBNF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\""))
  )
;xargs grep -nIE --color=always \""s"\" | sed \"s#/cyg[^:]*/##\""))
)
;;  } /* nll-grep-find */                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              nhl-grep-find                                         ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             grep perso                                            ;;
;;  {                                                                         ;;
(defun nhl-grep-find ()
  "Search word (egrep regex) in all files for NBNF_HL project."
  (interactive)
  (let (word prompt input)
    (setq word (clt-select-word))
    (if word
      (setq prompt (concat "Grep word NBNF_HL project: (default " word ") "))
      (setq prompt "Grep word (NBNF_HL): "))

    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))

    (grep (concat "cat d:/ccm_wa/NBNF/NBNF_HL/NBNF_CLIENT_HL/*.files | xargs grep --color=always -nsIE \""word"\""))
  )
)
;;  } /* nhl-grep-find */                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecar-grep-find                                        ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             grep perso                                            ;;
;;  {                                                                         ;;
(defun ecar-grep-find ()
  "Search word (egrep regex) in all files for ECAR project."
  (interactive)
  (let (word prompt input)
    (setq word (clt-select-word))
    (if word
      (setq prompt (concat "Grep word ECAR project: (default " word ") "))
      (setq prompt "Grep word (ECAR): "))

    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))

    (grep (concat "cat d:/ccm_wa/ECAR/ENSF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\""))
  )
)
;;  } /* ecar-grep-find */                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              grep-project-tmp                                      ;;
;;  @CREATED BY:        Personne_146                                          ;;
;;  @ABOUT:             grep perso                                            ;;
;;  {                                                                         ;;
;(defun grep-find-project-tmp (s)
;  (interactive "sSearch project tmp for: ")
;  (grep (concat "cat d:/ccm_wa/..._CLIENT/*.files | xargs grep --color=always -nsIE \""s"\""))
;  )
;;  } /* grep-project-tmp */                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ) ;; Magneti Marelli
) ;; cond ----------------------------------------------------------------------


;;;;;;;;;
;; ECB ;;
;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecb-toggle-width                                      ;;
;;  @CREATED BY:        Personne_146                                          ;;
;;  @ABOUT:             augmente/diminue la largeur de ecb                    ;;
;;  {                                                                         ;;
(defun ecb-toggle-width ()
  "Toggle variable ecb-window-width between 10% and 25%."
  (interactive)
  (set-variable 'ecb-windows-width (if (= ecb-windows-width 0.1) 0.25 0.1))
  (ecb-redraw-layout)
  (message "Ecb width set to %d." ecb-windows-width)
  )
;;  } /* ecb-toggle-width */                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecb-myopen-history                                    ;;
;;  @CREATED BY:        Personne_146                                          ;;
;;  @ABOUT:             ouvre l'historique ecb meme si il n'est pas visible   ;;
;;  {                                                                         ;;
(defun ecb-myopen-history ()
  "Open ecb-history."
  (interactive)
;  (ecb-show-ecb-windows)
  (ecb-goto-window-history)
  (message "Ecb History.")
  )
;;  } /* ecb-myopen-history */                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecb-myopen-directories                                ;;
;;  @CREATED BY:        Personne_146                                          ;;
;;  @ABOUT:             ouvre l'arborescence ecb                              ;;
;;                      meme si elle n'est pas visible                        ;;
;;  {                                                                         ;;
(defun ecb-myopen-directories ()
  "Open ecb-directories."
  (interactive)
;  (ecb-show-ecb-windows)
  (ecb-goto-window-directories)
  (message "Ecb Directories.")
  )
;;  } /* ecb-myopen-directories */                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecb-myopen-sources                                    ;;
;;  @CREATED BY:        Personne_146                                          ;;
;;  @ABOUT:             ouvre les fichiers source ecb                         ;;
;;                      meme si ce n'est pas visible                          ;;
;;  {                                                                         ;;
(defun ecb-myopen-sources ()
  "Open ecb-sources."
  (interactive)
;  (ecb-show-ecb-windows)
  (ecb-goto-window-sources)
  (message "Ecb Sources.")
  )
;;  } /* ecb-myopen-sources */                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecb-myopen-methods                                    ;;
;;  @CREATED BY:        Personne_146                                          ;;
;;  @ABOUT:             ouvre les fonctions ecb                               ;;
;;                      meme si ce n'est pas visible                          ;;
;;  {                                                                         ;;
(defun ecb-myopen-methods ()
  "Open ecb-methods."
  (interactive)
;  (ecb-show-ecb-windows)
  (ecb-goto-window-methods)
  (message "Ecb Methods.")
  )
;;  } /* ecb-myopen-methods */                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;
;; BUFFER CYCLE ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              next-user-buffer                                      ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             navigue dans les buffers sans prendre les *name*      ;;
;;  {                                                                         ;;
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.  User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "\\(^\\*\\|TAGS\\)" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))
;;  } /* next-user-buffer */                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              previous-user-buffer                                  ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             navigue dans les buffers sans prendre les *name*      ;;
;;  {                                                                         ;;
(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.  User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "\\(^\\*\\|TAGS\\)" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))
;;  } /* previous-user-buffer */                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;
;; START UP ;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              mystart-up                                            ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             lance ce qu'il faut au demarrage                      ;;
;;  {                                                                         ;;
(defun mystart-up ()
  "Start all mode necessary to work."
  (interactive)
  ;; to wait the maximize of the main window                                  ;;
  (custom-set-variables
   '(ecb-compile-window-height 25)
   '(ecb-windows-width 0.1))
  (ecb-activate)
  )
;;  } /* mystart-up */                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;
;; CONFIG ;;
;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              cfg-noob                                              ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             putain de soulard                                     ;;
;;  {                                                                         ;;
(defun cfg-noob ()
  "Configure GNU/Emacs for whose seem to want work."
  (interactive)
  (load-file (concat dotemacs-path "/dotemacs/noob.el"))
  )
;;  } /* cfg-noob */                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              cfg-classic                                           ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             putain de soulard                                     ;;
;;  {                                                                         ;;
(defun cfg-classic ()
  "Configure GNU/Emacs for whose want work."
  (interactive)
  (load-file (concat dotemacs-path "/emacs.el"))
  )
;;  } /* cfg-classic */                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              try-require                                           ;;
;;  @CREATED BY:        Fabrice Niessen                                       ;;
;;  @ABOUT:             attempt to load a feature/library, failing silently   ;;
;;  {                                                                         ;;
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
;;  } /* try-require */                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;
;; SEARCH ERROR ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              search-tab                                            ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             search a tab in buffer                                ;;
;;  {                                                                         ;;
(defun search-tab ()
  "Search a tab in the current buffer."
  (interactive)
  (occur "[\t\v]")
)
;;  } /* push-line */                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
;; Magneti Marelli--------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              search-fault                                          ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             search a fault in buffer                              ;;
;;  {                                                                         ;;
(defun search-fault ()
  "Search a fault in the current buffer."
  (interactive)
  ;; no space after ( after keyword                                           ;;
  (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\)[ ]*([ ]+" 'hi-green)
  ;; no space before ) after keyword                                          ;;
  (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\)[ ]*([^()]*[ ]+)" 'hi-green)
  ;; missing space after keyword                                              ;;
  (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\|#if\\|#endif\\)[^ ]*(" 'hi-yellow)
  ;; too space after/before parentheses                                       ;;
;  (highlight-regexp "^[^/\\n]*([ ]\\{1,\\}" 'hi-yellow)
;  (highlight-regexp "^[^/\\n]*[ ]\\{1,\\})" 'hi-yellow)
  (highlight-regexp "^[^/\\n]*[A-Za-z0-9]\\{4,7\\}_\\w[ ]\\{1,\\}(.*[),]" 'hi-yellow)
  ;; wrong type used                                                          ;;
  (highlight-regexp "^[^/\\n]*[^us_]\\(byte\\|short\\|long\\))" 'hi-pink)
  ;; space before a ( for a call/declaration of function                      ;;
  (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]+(" 'hi-yellow)
  ;; space after a ( for a call/declaration of function                       ;;
  (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]*([ ]+[[:alnum:]_]*" 'hi-green)
  ;; space before a ) for a call/decalaration of function                     ;;
  (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]*([ ]*[[:alnum:]_]*\\([ ]+[[:alnum:]_]*\\|[ ]*([[:alnum:]_]*\\(,[[:alnum:]_]\\)*)\\)[ ]+)" 'hi-green)
)
;;  } /* search-fault */                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              search-fault-size                                     ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             search a fault in buffer                              ;;
;;  {                                                                         ;;
(defun search-fault-size ()
  "Search a sizing fault in the current buffer."
  (interactive)
  ;; wrong function header used                                               ;;
  (highlight-regexp "/\\**/" 'hi-blue)
  ;; line more than 80 column                                                 ;;
  (highlight-regexp ".\\{81,\\}" 'hi-green)
)
;;  } /* search-fault-size */                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ) ;; Magneti Marelli
) ;; cond ----------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;
;; COLOR TRACE CAN ;;
;;;;;;;;;;;;;;;;;;;;;

(cond
;; Magneti Marelli -------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              color-trace-can-immo                                  ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             color trace can for immo                              ;;
;;  {                                                                         ;;
(defun color-trace-can-immo ()
  "Color the trace in the current buffer."
  (interactive)
  ;; Response OK and NOK from IMMO                                            ;;
  (highlight-regexp " 0?17[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6\\}[ ]*[0-9A-F][13579BDF]" 'font-lock-function-name-face)
  (highlight-regexp " 0?17[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6\\}[ ]*[0-9A-F][02468ACE]" 'font-lock-preprocessor-face)
  ;; request from ELV                                                         ;;
  (highlight-regexp " 0?16[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-comment-face)
  ;; response from KESSY                                                      ;;
  (highlight-regexp " 29D[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6,\\}[ ]*[1-9A-F][0-9A-F]  [0-9A-F][0-9A-F]" 'font-lock-variable-name-face)
  (highlight-regexp " 29D[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-variable-name-face)
  ;; request from IMMO                                                        ;;
  (highlight-regexp " 155[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-string-face)
  (highlight-regexp " 157[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-string-face)
  ;; button start                                                             ;;
  (highlight-regexp " 5F1[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*[0-9A-F][89A-F]" 'font-lock-type-face)
  ;; KL 15 ON                                                                 ;;
  (highlight-regexp " 5F1[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  ;; KL S-Contact ON ;;
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF]" 'font-lock-keyword-face)
  ;; not recognized key                                                       ;;
  (highlight-regexp " 621[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}[ ]*[0-9A-F]0" 'compilation-warning)
  ;; recognised key                                                           ;;
  (highlight-regexp " 621[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}[ ]*[0-9A-F][1-9A-F]" 'font-lock-function-name-face)
)
;;  } /* color-trace-can-immo */                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              color-trace-can-cvsensor                              ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             color trace can for cv sensor                         ;;
;;  {                                                                         ;;
(defun color-trace-can-cvsensor ()
  "Color the trace in the current buffer."
  (interactive)
  ;; BRAKE                                                                    ;;
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*8[0-9A-F]" 'font-lock-type-face)
  ;; error                                                                    ;;
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*[9AB][0-9A-F]" 'font-lock-comment-face)
  ;; OFF                                                                      ;;
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*C[0-9A-F]" 'font-lock-preprocessor-face)
  ;; ON                                                                       ;;
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*D[0-9A-F]" 'font-lock-function-name-face)

  ;; KL 15 ON                                                                 ;;
  (highlight-regexp " 575[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  ;; KL 15 ON                                                                 ;;
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
)
;;  } /* color-trace-can-cvsensor */                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              color-trace-can-nmh                                   ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             color trace can for cv sensor                         ;;
;;  {                                                                         ;;
(defun color-trace-can-nmh ()
  "Color the trace in the current buffer."
  (interactive)
  ;; NMH message                                                              ;;
  (highlight-regexp " 72[4-9A-F][ ]*Tx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-type-face)
  (highlight-regexp " 73[0-9A-F][ ]*Tx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-type-face)

  ;; comment state
  (highlight-regexp "\"    START       \"" 'font-lock-variable-name-face)
  (highlight-regexp "\" READY_TO_SLEEP \"" 'font-lock-variable-name-face)
  (highlight-regexp "\"PREPARE_TO_SLEEP\"" 'font-lock-variable-name-face)
  (highlight-regexp "\"    SLEEP       \"" 'font-lock-variable-name-face)
  (highlight-regexp "\"    NORMAL      \"" 'font-lock-variable-name-face)

  (highlight-regexp "/\\*.*\\*/" 'font-lock-comment-face)

  ;; State of kombi bus                                                       ;;
  ;;  Sleep -> Start            (vert pastel)                                 ;;
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]1\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-function-name-face)

  ;;  PrepareToSleep -> Start   (cyan)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]2\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-keyword-face)

  ;;  Start -> Normal           (turquoise pastel)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]4\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'font-lock-constant-face)

  ;;  ReadyToSleep -> Normal    (bleu gris pastel)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]8\\([ ]*[0-9A-F][0-9A-F]\\)\\{3\\}" 'fonct-lock-builtin-face)
)
;;  } /* color-trace-can-nmh */                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              color-trace-can-curr                                  ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             color trace can for current problem                   ;;
;;  {                                                                         ;;
(defun color-trace-can-curr ()
  "Color the trace in the current buffer."
  (interactive)
  ;; mBSG_Kombi message KL58d != 0                                            ;;
  (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[1-9A-F][0-9A-F]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-type-face)
  (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[0-9A-F][1-9A-F]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-type-face)

  ;; mBSG_Kombi message KL58d = 0                                             ;;
  (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*00\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-variable-name-face)


  ;; mBSG_Last KL15 ON                                                       ;;
  (highlight-regexp " 570[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-string-face)

  ;; mBSG_Last KL15 OFF                                                       ;;
  (highlight-regexp " 570[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][014589CD]\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-constant-face)


  ;; mSystem_info1
  (highlight-regexp " 5D0[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][4567CDEF]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-comment-face)


  ;; comment
  (highlight-regexp "/\\*.*\\*/" 'font-lock-comment-face)


  ;; State of kombi bus                                                       ;;
  ;;  Sleep -> Start            (vert pastel)                                 ;;
  (highlight-regexp " 727[ ]*[RT]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]1\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-function-name-face)

  ;;  PrepareToSleep -> Start   (cyan)
  (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]2\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-keyword-face)

  ;;  Start -> Normal           (turquoise pastel)
  (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]4\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'font-lock-constant-face)

  ;;  ReadyToSleep -> Normal    (bleu gris pastel)
  (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]8\\([ ]*[0-9A-F][0-9A-F]\\)\\{3\\}" 'fonct-lock-builtin-face)
)
;;  } /* color-trace-can-nmh */                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              color-trace-can-fuel                                  ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             color trace can for fuel tank                         ;;
;;  {                                                                         ;;
(defun color-trace-can-fuel ()
  "Color the trace in the current buffer."
  (interactive)
  ;; mKombi_1                                                                 ;;
  ;; Tankinhalt                                                               ;;
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[1-9A-F][0-9A-F]" 'font-lock-comment-face)
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[0-9A-F][1-9A-F]" 'font-lock-comment-face)
  ;; Tankstop                                                                 ;;
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF][ ]*[89ABCDEF]" 'font-lock-constant-face)
  ;; Sta_tank                                                                 ;;
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF]" 'font-lock-function-name-face)
  ;; Warn_tank                                                                ;;
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[4567CDEF][0-9A-F]" 'font-lock-type-face)

  ;; mMFA_1
  ;; Reichweite                                                               ;;
  (highlight-regexp " 629[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-preprocessor-face)

  ;; mMFA_1
  ;; Verbrauch                                                                ;;
  (highlight-regexp " 62B[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'compilation-warning)

  ;; KL 15 ON                                                                 ;;
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
)
;;  } /* color-trace-can-fuel */                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              color-trace-can-erange                                ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             color trace can for electric range                    ;;
;;  {                                                                         ;;
(defun color-trace-can-erange ()
  "Color the trace in the current buffer."
  (interactive)
  ;; battery charge in percent in mMotor_EV1                                  ;;
  (highlight-regexp " 61[Aa][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}[ ]*[0-9A-F][0-9A-F]" 'font-lock-comment-face)
  ;; battery charge in kWh in mMotor_EV1                                      ;;
  (highlight-regexp " 61[Aa][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}[ ]*[0-9A-F][0-9A-F][ ]*[0-9A-F][0-9A-F]" 'font-lock-type-face)
  ;; consumption in mMotor_15                                                 ;;
  (highlight-regexp " 57[Ff][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{8\\}" 'font-lock-function-name-face)
  ;; ON                                                                       ;;
;  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*D[0-9A-F]" 'font-lock-function-name-face)

  ;; KL 15 ON                                                                 ;;
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
)
;;  } /* color-trace-can-erange */                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ) ;; Magneti Marelli
) ;; cond ----------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;
;; ALIGN C MODE ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              align-function                                        ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             align variable in declaration of function             ;;
;;  {                                                                         ;;
;(defun align-function (beg end)
;  "Align variable in declaration of function."
;  (interactive "r")
;  (unless (and beg end)
;    (error "The mark is not set now, so there is no region"))
;;  (shell-command-on-region b e "figlet" (current-buffer) t)
;  (align-regexp beg end "[A-Za-z0-9_\\*]*\\(\\[[0-9]*\\]\\)\\{0,1\\}[,;)]\\{1,2\\}")
;  ; [A-Za-z0-9_\*]*\(\[[0-9]*\]\)\{0,1\}[,;)]\{1,2\}
;)
;;  } /* align-function */                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              align-variable                                        ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             align name in declaration of variable                 ;;
;;  {                                                                         ;;
;(defun align-variable (beg end)
;  "Align name in declaration of variable."
;  (interactive "r")
;  (unless (and beg end)
;    (error "The mark is not set now, so there is no region"))
;  (let (regex "[=+-/\\*!]\\{0,1\\}= [A-Za-z0-9_\\*]*\\(\\[[0-9]*\\]\\)\\{0,1\\}")
;    (align-regexp beg end 'regex))
;  ; [=+-/\*!]\{0,1\}= [A-Za-z0-9_\*]*\(\[[0-9]*\]\)\{0,1\}
;;  (insert marker-position region-end)
;)
;;  } /* align-variable */                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;
;; ALIGN RTRT MODE ;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              rtrt-align-init                                       ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             align init in ptu script for rtrt                     ;;
;;  {                                                                         ;;
(defun rtrt-align-init (start end)
  "Align init variable test case (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (align-regexp start end (concat "\\(\\s-*\\)" "\\binit ") 1 1)
)
;;  } /* rtrt-align-init */                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              rtrt-align-ev                                         ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             align expected value in ptu script for rtrt           ;;
;;  {                                                                         ;;
(defun rtrt-align-ev (start end)
  "Align expected value variable (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (align-regexp start end (concat "\\(\\s-*\\)" "\\bev ") 1 1)
)
;;  } /* rtrt-align-ev */                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              rtrt-align-declaration                                ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             align expected value in ptu script for rtrt           ;;
;;  {                                                                         ;;
(defun rtrt-align-declaration (start end)
  "Align variable (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (rtrt-align-init start end)
  (rtrt-align-ev start end)
)
;;  } /* rtrt-align-declaration */                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;
;; FORMAT RTRT MODE ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              rtrt-remove-whitespace-before-colon                   ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             remove withespace before a colon in ptu script for    ;;
;;                      RTRT                                                  ;;
;;  {                                                                         ;;
(defun rtrt-remove-whitespace-before-colon (start end)
  "Remove all space before a colon (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (replace-regexp "\\s-+," "," nil start end)
)
;;  } /* rtrt-remove-whitespace-before-colon */                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              rtrt-replace-var-string                               ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             align init in ptu script for rtrt                     ;;
;;  {                                                                         ;;
(defun rtrt-upcase-var-string (start end)
  "Upcase the var string (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (replace-regexp "[vV]ar " "VAR " nil start end)
)
;;  } /* rtrt-replace-var-string */                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;
;; END OF LINE ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              dos2unix                                              ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             convert MS-DOS format \r\n to Unix format \n          ;;
;;  {                                                                         ;;
(defun dos2unix ()
  "Transform a DOS file to a Unix file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")
  )
)
;;  } /* dos2unix */                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              unix2dos                                              ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             convert Unix format \n to MS-DOS format \r\n          ;;
;;  {                                                                         ;;
(defun unix2dos ()
  "Transform a Unix file to a DOS file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")
  )
)
;;  } /* unix2dos */                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;
;; TRANSPARENCY ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              toggle-transparency                                   ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             toggle the window transparency to alpha 100->85->100  ;;
;;  {                                                                         ;;
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  "Toggle the transparency of whole Emacs window."
  (interactive)
  (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
    (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
;;  } /* toggle-transparency */                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;
;; C-EXPAND-MACRO ;;
;;;;;;;;;;;;;;;;;;;;

(cond
;; Magneti Marelli -------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              nsf-c-expand-macro                                    ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             expand macro for project NSF                          ;;
;;  {                                                                         ;;
(defun nsf-c-expand-macro (start end)
  "Expand macro in C language for the NSF project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags clt-nsf-flag)
  (c-macro-expand start end nil)
)
;;  } /* nsf-c-expand-macro */                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              nhl-c-expand-macro                                    ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             expand macro for project NBNF_HL                      ;;
;;  {                                                                         ;;
(defun nhl-c-expand-macro (start end)
  "Expand macro in C language for the NBNF_HL project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags clt-nbnfhl-flag)
  (c-macro-expand start end nil)
)
;;  } /* nhl-c-expand-macro */                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              nll-c-expand-macro                                    ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             expand macro for project NBNF_LL                      ;;
;;  {                                                                         ;;
(defun nll-c-expand-macro (start end)
  "Expand macro in C language for the NBNF_LL project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags clt-nbnfll-flag)
  (c-macro-expand start end nil)
)
;;  } /* nll-c-expand-macro */                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ecar-c-expand-macro                                   ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             expand macro for project ECAR                         ;;
;;  {                                                                         ;;
(defun ecar-c-expand-macro (start end)
  "Expand macro in C language for the ECAR project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags clt-ecar-flag)
  (c-macro-expand start end nil)
)
;;  } /* ecar-c-expand-macro */                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              xl1-c-expand-macro                                    ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             expand macro for project XL1                          ;;
;;  {                                                                         ;;
(defun xl1-c-expand-macro (start end)
  "Expand macro in C language for the XL1 project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags clt-xl1-flag)
  (c-macro-expand start end nil)
)
;;  } /* xl1-c-expand-macro */                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ) ;; Magneti Marelli
) ;; cond ----------------------------------------------------------------------



;;;;;;;;;;
;; TEST ;;
;;;;;;;;;;

(defun clt-test ()
  "Plein de test."
  (interactive)
  (insert (concat "toto=" shell-file-name "$"))
)

(setq semantic-c-takeover-hideif t)

(cond
;; Magneti Marelli -------------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
(fset 'header-function
   [home right right ?\C-7 ?\C-6 delete ?\C-7 ?\C-6 kp-subtract home])
(global-set-key (kbd "C-c '") 'header-function)

(fset 'delete-space-before-function-parenthese
   [home ?\C-s ?\( left backspace home])
(global-set-key (kbd "C-c ;") 'delete-space-before-function-parenthese)
  ) ;; Magneti Marelli
) ;; cond ----------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              push-line                                             ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             Copy entire line in kill ring (whitout Home C-k C-y)  ;;
;;  {                                                                         ;;
(defun push-line ()
  "Select current line, push onto kill ring."
  (interactive)
  (save-excursion
    (copy-region-as-kill (re-search-backward "^") (re-search-forward "$"))
  )
)
;;  } /* push-line */                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              toggle-source-header                                  ;;
;;  @CREATED BY:        Jan Borsodi                                           ;;
;;  @ABOUT:             ??                                                    ;;
;;  {                                                                         ;;
;; Switches between source/header files
(defun toggle-source-header()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((buf (current-buffer))
	(name (file-name-nondirectory (buffer-file-name)))
	file
	offs)
    (setq offs (string-match c++-header-ext-regexp name))
    (if offs
	(let ((lst c++-source-extension-list)
	      (ok nil)
	      ext)
	  (setq file (substring name 0 offs))
	  (while (and lst (not ok))
	    (setq ext (car lst))
	    (if (file-exists-p (concat file "." ext))
		  (setq ok t))
	    (setq lst (cdr lst)))
	  (if ok
	      (find-file (concat file "." ext))))
      (let ()
	(setq offs (string-match c++-source-ext-regexp name))
	(if offs
	    (let ((lst c++-header-extension-list)
		  (ok nil)
		  ext)
	      (setq file (substring name 0 offs))
	      (while (and lst (not ok))
		(setq ext (car lst))
		(if (file-exists-p (concat file "." ext))
		    (setq ok t))
		(setq lst (cdr lst)))
	      (if ok
		  (find-file (concat file "." ext)))))))))
;;  } /* toggle-source-header */                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;list-colors-display to display all color


;; S-RET leaves lazy-highlighted matches.
;(defun my-isearch-exit-leave-lazy-highlight ()
;  "Exit search and leave extra match highlighting."
;  (interactive)
;  (let ((lazy-highlight-cleanup nil))
;    (when isearch-lazy-highlight
;      (isearch-lazy-highlight-new-loop (point-min) (point-max)))
;    (isearch-exit)))

;(define-key isearch-mode-map [(shift return)]
;                             'my-isearch-exit-leave-lazy-highlight)

;; C-RET doesn't add the current search string to the history.
;(add-hook 'isearch-mode-end-hook
;          (lambda ()
;            ;; On typing C-RET
;            (when (eq last-input-char 'C-return)
;              ;; Set the point at the beginning of the search string
;              (if (and isearch-forward isearch-other-end)
;                  (goto-char isearch-other-end))
;              ;; Don't push the search string into the search ring
;              (if isearch-regexp
;                  (setq regexp-search-ring (cdr regexp-search-ring))
;                (setq search-ring (cdr search-ring))))))

;; Use ScrollLock key to activate scroll-lock-mode
;; (from http://lists.gnu.org/archive/html/emacs-devel/2005-06/msg01274.html)
;(let ((key (if (eq window-system 'w32) "<scroll>" "<Scroll_Lock>")))
;  (unless (lookup-key (current-global-map) (read-kbd-macro key))
;    (define-key (current-global-map) (read-kbd-macro key) 'scroll-lock-mode)))


;;;;;;;;;;;
;; GTAGS ;;
;;;;;;;;;;;

;; to put in gtags.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              gtags-current-buffer                                  ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             get a pattern from current buffer name                ;;
;;  {                                                                         ;;
;(defun gtags-current-buffer ()
;  (interactive)
;  (setq buffername (buffer-name))
;  (setq buffername (substring buffername 0 4))
;;  (insert (concat "toto=" buffername ";"))
;)
;;;  } /* gtags-current-buffer */                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to put in gtags.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              gtags-find-file-custom                                ;;
;;  @CREATED BY:        Claude TETE                                           ;;
;;  @ABOUT:             find a related file from a pattern of current buffer  ;;
;;  {                                                                         ;;
;(defun gtags-find-file-custom ()
;  "Input pattern and move to the top of the file."
;  (interactive)
;  (let (tagname prompt input)
;    (setq tagname (gtags-current-buffer))
;    (if tagname
;      (setq prompt (concat "Find related files: (default " tagname ") "))
;      (setq prompt "Find related files: "))
;    (setq input (completing-read prompt 'gtags-completing-files
;                  nil nil nil gtags-history-list))
;    (if (not (equal "" input))
;      (setq tagname input))
;    (gtags-push-context)
;    (gtags-goto-tag tagname "Po")))
;(global-set-key (kbd "C-c t") 'toggle-transparency)
;;  } /* gtags-find-file-custom */                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              ww-next-gtag                                          ;;
;;  @CREATED BY:        William Wong                                          ;;
;;  @ABOUT:             cycling through multiple tags results                 ;;
;;  {                                                                         ;;
;(when section-mode-gnu-global
;(defun ww-next-gtag ()
;  "Find next matching tag, for GTAGS."
;  (interactive)
;  (let ((latest-gtags-buffer
;         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
;                                 (buffer-list)) ))))
;    (cond (latest-gtags-buffer
;           (switch-to-buffer latest-gtags-buffer)
;           (next-line)
;           (gtags-select-it nil))
;          ) ))
;)
;;  } /* ww-next-gtag */                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I don't have tested
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonction                                                                  ;;
;;  @NAME:              gtags-global-update                                   ;;
;;  @CREATED BY:        Kevin Rodgers                                         ;;
;;  @ABOUT:             cyclin through multiple tags results                 ;;
;;  {                                                                         ;;
;(when (locate-library "gtags")
;
;  (autoload 'gtags-mode "gtags" nil t)
;
;  (when (executable-find "global")
;
;    (defadvice gtags-visit-rootdir (after make-complete-list activate)
;      "Rebuilds completion list when changing GLOBAL database rootdir."
;      (gtags-make-complete-list))
;
;    (defun gtags-global-dir-p (dir)
;      "Return non-nil if directory DIR contains a GLOBAL database."
;      (and (file-exists-p (expand-file-name "GPATH" dir))
;           (file-exists-p (expand-file-name "GRTAGS" dir))
;           (file-exists-p (expand-file-name "GSYMS" dir))
;           (file-exists-p (expand-file-name "GTAGS" dir))))
;
;    (defun gtags-global-dir (&optional dir)
;      "Return the nearest super directory that contains a GLOBAL database."
;      (interactive)
;      (when (null dir)
;        (setq dir default-directory))
;      (cond ((gtags-global-dir-p dir) dir)
;            ((equal (file-truename dir) (file-truename "/")) nil)
;            (t (gtags-global-dir (file-name-as-directory
;                                  (expand-file-name ".." dir))))))
;
;    (defvar gtags-global-complete-list-obsolete-flag nil
;      "When non-nil, the GLOBAL complete list should be rebuilt.")
;
;    (defun gtags-global-update ()
;      "If current directory is part of a GLOBAL database update it."
;      (interactive)
;      (when (gtags-global-dir)
;        (if (equal (call-process "global" nil nil nil "-vu") 0)
;            (setq gtags-global-complete-list-obsolete-flag t)
;          (error "Global database update failed"))))
;
;    (defun gtags-global-complete-list-maybe ()
;      "Rebuild the GLOBAL complete list when indicated.
;See `gtags-global-complete-list-obsolete-flag'."
;      (interactive)
;      (when gtags-global-complete-list-obsolete-flag
;        (gtags-make-complete-list)
;        (setq gtags-global-complete-list-obsolete-flag nil)))
;
;    (add-hook 'gtags-mode-hook
;              (lambda ()
;                (add-hook 'after-save-hook 'gtags-global-update nil t)
;                (defadvice gtags-find-tag
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-rtag
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-symbol
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-pattern
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-with-grep
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-with-idutils
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-file
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-parse-file
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                (defadvice gtags-find-tag-from-here
;                  (before gtags-global-complete-list-maybe activate)
;                  (gtags-global-complete-list-maybe))
;                )                       ; (lambda () ...)
;              )                        ; (add-hook 'gtags-mode-hook ...)
;    )                            ; (when (executable-find "global") ...)
;
;  ;; Use gtags in all modes for now.
;  (gtags-mode 1)
;  )                                ; (when (locate-library "gtags") ...)
;

;;; functions.el ends here
