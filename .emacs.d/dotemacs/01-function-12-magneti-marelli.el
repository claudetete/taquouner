;;; 01-function-12-magneti-marelli.el --- add some function for Magneti Marelli profile

;; Copyright (c) 2010-2019 Claude Tete
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
;; Version: 0.4
;; Created: June 2010
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom functions about Magneti Marelli specific needs]
;; [SUBDEFAULT.nil]


;;; Code:
;;;  CONST
(defconst mm-nsf-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -DC_COMP_COSMIC_MC9S12 -D__MC9S12xx__ -D__MC9S12XHZ__ -I d:/ccm_wa/NSF/NSF/NSF_CLIENT/out/include")

(defconst mm-nbnfhl-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/NBNF/NBNF_HL/NBNF_CLIENT_HL/out/include")

(defconst mm-nbnfll-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -DC_COMP_COSMIC_MC9S12 -D__MC9S12xx__ -D__MC9S12XHZ__ -I d:/ccm_wa/NBNF/NBNF_LL/NSFNBNF_CLIENT/out/include")

(defconst mm-ecar-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/ECAR/ENSF_CLIENT/out/include")

(defconst mm-xl1-flag  "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__ -D__NEC_V850__ -D__NEC_V850_Dx3__ -D__NEC_V850_DL3__ -D__NEC_V850_DL3_F3427__ -DC_COMP_GHS_V850 -DC_COMP_GHS_V85X -I d:/ccm_wa/XL1/XL1_CLIENT/out/include")

;;
;; GREP-FIND
;; grep custom (by Claude TETE)
(defun nsf-grep-find ()
  "Search word (egrep regex) in all files for NSF project."
  (interactive)
  (let (word prompt input)
    (setq word (tqnr-select-word))
    (if word
      (setq prompt (concat "Grep word NSF project: (default " word ") "))
      (setq prompt "Grep word (NSF): "))
    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))
    (grep (concat "cat d:/ccm_wa/NSF/NSF/NSF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\"")))
  )

;;; grep custom (by Claude TETE)
(defun nll-grep-find ()
  "Search word (egrep regex) in all files for NBNF LL project."
  (interactive)
  (let (word prompt input)
    (setq word (tqnr-select-word))
    (if word
      (setq prompt (concat "Grep word NBNF_LL project: (default " word ") "))
      (setq prompt "Grep word (NBNF_LL): "))
    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))
    (grep (concat "cat d:/ccm_wa/NBNF/NBNF_LL/NSFNBNF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\""))
    )
  )

;;; grep custom (by Claude TETE)
(defun nhl-grep-find ()
  "Search word (egrep regex) in all files for NBNF_HL project."
  (interactive)
  (let (word prompt input)
    (setq word (tqnr-select-word))
    (if word
      (setq prompt (concat "Grep word NBNF_HL project: (default " word ") "))
      (setq prompt "Grep word (NBNF_HL): "))
    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))
    (grep (concat "cat d:/ccm_wa/NBNF/NBNF_HL/NBNF_CLIENT_HL/*.files | xargs grep --color=always -nsIE \""word"\""))
    )
  )

;;; grep custom (by Claude TETE)
(defun ecar-grep-find ()
  "Search word (egrep regex) in all files for ECAR project."
  (interactive)
  (let (word prompt input)
    (setq word (tqnr-select-word))
    (if word
      (setq prompt (concat "Grep word ECAR project: (default " word ") "))
      (setq prompt "Grep word (ECAR): "))
    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))
    (grep (concat "cat d:/ccm_wa/ECAR/ENSF_CLIENT/*.files | xargs grep --color=always -nsIE \""word"\""))
    )
  )

;;
;; SEARCH ERROR
;; search a fault in buffer (by Claude TETE)
(defun search-fault ()
  "Search a fault in the current buffer."
  (interactive)
  ;; no space after ( after keyword
  (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\)[ ]*([ ]+" 'hi-green)
  ;; no space before ) after keyword
  (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\)[ ]*([^()]*[ ]+)" 'hi-green)
  ;; missing space after keyword
  (highlight-regexp "\\<\\(if\\|while\\|for\\|return\\|switch\\|#if\\|#endif\\)[^ ]*(" 'hi-yellow)
      ;;;; too space after/before parentheses
  ;;(highlight-regexp "^[^/\\n]*([ ]\\{1,\\}" 'hi-yellow)
  ;;(highlight-regexp "^[^/\\n]*[ ]\\{1,\\})" 'hi-yellow)
  (highlight-regexp "^[^/\\n]*[A-Za-z0-9]\\{4,7\\}_\\w[ ]\\{1,\\}(.*[),]" 'hi-yellow)
  ;; wrong type used
  (highlight-regexp "^[^/\\n]*[^us_]\\(byte\\|short\\|long\\))" 'hi-pink)
  ;; space before a ( for a call/declaration of function
  (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]+(" 'hi-yellow)
  ;; space after a ( for a call/declaration of function
  (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]*([ ]+[[:alnum:]_]*" 'hi-green)
  ;; space before a ) for a call/declaration of function
  (highlight-regexp "[[:alnum:]]\\{4,\\}_[[:alnum:]_]+[ ]*([ ]*[[:alnum:]_]*\\([ ]+[[:alnum:]_]*\\|[ ]*([[:alnum:]_]*\\(,[[:alnum:]_]\\)*)\\)[ ]+)" 'hi-green)
  )

;; search a fault size in buffer (by Claude TETE)
(defun search-fault-size ()
  "Search a sizing fault in the current buffer."
  (interactive)
  ;; wrong function header used
  (highlight-regexp "/\\**/" 'hi-blue)
  ;; line more than 80 column
  (highlight-regexp ".\\{81,\\}" 'hi-green)
  )

;;
;;;
;;;; COLOR TRACE CAN
;;; color CAN trace for immo (by Claude TETE)
(defun color-trace-can-immo ()
  "Color the trace in the current buffer."
  (interactive)
  ;; Response OK and NOK from IMMO
  (highlight-regexp " 0?17[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6\\}[ ]*[0-9A-F][13579BDF]" 'font-lock-function-name-face)
  (highlight-regexp " 0?17[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6\\}[ ]*[0-9A-F][02468ACE]" 'font-lock-preprocessor-face)
  ;; request from ELV
  (highlight-regexp " 0?16[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-comment-face)
  ;; response from KESSY
  (highlight-regexp " 29D[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{6,\\}[ ]*[1-9A-F][0-9A-F]  [0-9A-F][0-9A-F]" 'font-lock-variable-name-face)
  (highlight-regexp " 29D[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-variable-name-face)
  ;; request from IMMO
  (highlight-regexp " 155[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-string-face)
  (highlight-regexp " 157[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F]" 'font-lock-string-face)
  ;; button start
  (highlight-regexp " 5F1[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*[0-9A-F][89A-F]" 'font-lock-type-face)
  ;; KL 15 ON
  (highlight-regexp " 5F1[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  ;; KL S-Contact ON ;;
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF]" 'font-lock-keyword-face)
  ;; not recognized key
  (highlight-regexp " 621[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}[ ]*[0-9A-F]0" 'compilation-warning)
  ;; recognised key
  (highlight-regexp " 621[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}[ ]*[0-9A-F][1-9A-F]" 'font-lock-function-name-face)
  )

;;; color CAN trace for cv sensor (by Claude TETE)
(defun color-trace-can-cvsensor ()
  "Color the trace in the current buffer."
  (interactive)
  ;; BRAKE
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*8[0-9A-F]" 'font-lock-type-face)
  ;; error
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*[9AB][0-9A-F]" 'font-lock-comment-face)
  ;; OFF
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*C[0-9A-F]" 'font-lock-preprocessor-face)
  ;; ON
  (highlight-regexp " 366[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][0-9A-F][ ]*D[0-9A-F]" 'font-lock-function-name-face)
  ;;
  ;; KL 15 ON
  (highlight-regexp " 575[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  ;; KL 15 ON
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  )

;;; color CAN trace for NMH (by Claude TETE)
(defun color-trace-can-nmh ()
  "Color the trace in the current buffer."
  (interactive)
  ;; NMH message
  (highlight-regexp " 72[4-9A-F][ ]*Tx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-type-face)
  (highlight-regexp " 73[0-9A-F][ ]*Tx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-type-face)
  ;;
  ;; comment state
  (highlight-regexp "\"    START       \"" 'font-lock-variable-name-face)
  (highlight-regexp "\" READY_TO_SLEEP \"" 'font-lock-variable-name-face)
  (highlight-regexp "\"PREPARE_TO_SLEEP\"" 'font-lock-variable-name-face)
  (highlight-regexp "\"    SLEEP       \"" 'font-lock-variable-name-face)
  (highlight-regexp "\"    NORMAL      \"" 'font-lock-variable-name-face)
  ;;
  (highlight-regexp "/\\*.*\\*/" 'font-lock-comment-face)
  ;;
  ;; State of kombi bus
  ;;  Sleep -> Start            (pale green)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]1\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-function-name-face)
  ;;
  ;;  PrepareToSleep -> Start   (cyan)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]2\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-keyword-face)
  ;;
  ;;  Start -> Normal           (pale turquoise)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]4\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'font-lock-constant-face)
  ;;
  ;;  ReadyToSleep -> Normal    (pale grey blue)
  (highlight-regexp " 727[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]8\\([ ]*[0-9A-F][0-9A-F]\\)\\{3\\}" 'fonct-lock-builtin-face)
  )

;;; color CAN trace for current problem (by Claude TETE)
(defun color-trace-can-curr ()
  "Color the trace in the current buffer."
  (interactive)
  ;; mBSG_Kombi message KL58d != 0
  (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[1-9A-F][0-9A-F]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-type-face)
  (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[0-9A-F][1-9A-F]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-type-face)
  ;;
  ;; mBSG_Kombi message KL58d = 0
  (highlight-regexp " 470[ ]*[TR]x[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*00\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}" 'font-lock-variable-name-face)
  ;;
  ;; mBSG_Last KL15 ON
  (highlight-regexp " 570[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-string-face)
  ;;
  ;; mBSG_Last KL15 OFF
  (highlight-regexp " 570[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][014589CD]\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-constant-face)
  ;;
  ;; mSystem_info1
  (highlight-regexp " 5D0[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][4567CDEF]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}" 'font-lock-comment-face)
  ;;
  ;; comment
  (highlight-regexp "/\\*.*\\*/" 'font-lock-comment-face)
  ;;
  ;; State of kombi bus
  ;;  Sleep -> Start            (pale green)
  (highlight-regexp " 727[ ]*[RT]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]1\\([ ]*[0-9A-F][0-9A-F]\\)\\{1\\}" 'font-lock-function-name-face)
  ;;
  ;;  PrepareToSleep -> Start   (cyan)
  (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]2\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-keyword-face)
  ;;
  ;;  Start -> Normal           (pale turquoise)
  (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]4\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'font-lock-constant-face)
  ;;
  ;;  ReadyToSleep -> Normal    (pale grey blue)
  (highlight-regexp " 727[ ]*[TR]x[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F]8\\([ ]*[0-9A-F][0-9A-F]\\)\\{3\\}" 'fonct-lock-builtin-face)
  )

;;; color CAN trace for fuel tank (by Claude TETE)
(defun color-trace-can-fuel ()
  "Color the trace in the current buffer."
  (interactive)
  ;; mKombi_1
  ;; Tankinhalt
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[1-9A-F][0-9A-F]" 'font-lock-comment-face)
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}[ ]*[0-9A-F][1-9A-F]" 'font-lock-comment-face)
  ;; Tankstop
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF][ ]*[89ABCDEF]" 'font-lock-constant-face)
  ;; Sta_tank
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][13579BDF]" 'font-lock-function-name-face)
  ;; Warn_tank
  (highlight-regexp " 320[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[4567CDEF][0-9A-F]" 'font-lock-type-face)
  ;;
  ;; mMFA_1
  ;; Reichweite
  (highlight-regexp " 629[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{4\\}" 'font-lock-preprocessor-face)
  ;;
  ;; mMFA_1
  ;; Verbrauch
  (highlight-regexp " 62B[ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{2\\}" 'compilation-warning)
  ;;
  ;; KL 15 ON
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  )

;;; color CAN trace for electric range (by Claude TETE)
(defun color-trace-can-erange ()
  "Color the trace in the current buffer."
  (interactive)
  ;; battery charge in percent in mMotor_EV1
  (highlight-regexp " 61[Aa][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{7\\}[ ]*[0-9A-F][0-9A-F]" 'font-lock-comment-face)
  ;; battery charge in kWh in mMotor_EV1
  (highlight-regexp " 61[Aa][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{5\\}[ ]*[0-9A-F][0-9A-F][ ]*[0-9A-F][0-9A-F]" 'font-lock-type-face)
  ;; consumption in mMotor_15
  (highlight-regexp " 57[Ff][ ]*Rx[ ]*[Dd][ ]*[0-9]\\([ ]*[0-9A-F][0-9A-F]\\)\\{8\\}" 'font-lock-function-name-face)
  ;;
  ;; KL 15 ON
  (highlight-regexp " 570[ ]*Rx[ ]*[Dd][ ]*[0-9][ ]*[0-9A-F][2367ABEF]" 'font-lock-constant-face)
  )

;;
;;;
;;;; C-EXPAND-MACRO
;;; expand macro for project NSF (by Claude TETE)
(defun nsf-c-expand-macro (start end)
  "Expand macro in C language for the NSF project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags mm-nsf-flag)
  (c-macro-expand start end nil)
  )

;;; expand macro for project NBNF HL (by Claude TETE)
(defun nhl-c-expand-macro (start end)
  "Expand macro in C language for the NBNF_HL project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags mm-nbnfhl-flag)
  (c-macro-expand start end nil)
  )

;;; expand macro for project NBNF LL (by Claude TETE)
(defun nll-c-expand-macro (start end)
  "Expand macro in C language for the NBNF_LL project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags mm-nbnfll-flag)
  (c-macro-expand start end nil)
  )

;;; expand macro for project ENSF (by Claude TETE)
(defun ecar-c-expand-macro (start end)
  "Expand macro in C language for the ECAR project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags mm-ecar-flag)
  (c-macro-expand start end nil)
  )

;;; expand macro for project XL1 (by Claude TETE)
(defun xl1-c-expand-macro (start end)
  "Expand macro in C language for the XL1 project (between START and END)."
  (interactive "r")
  (unless (and start end)
    (error "The mark is not set now, so there is no region"))
  (setq c-macro-cppflags mm-xl1-flag)
  (c-macro-expand start end nil)
  )

;; macro for function header
(fset 'header-function
  [home right right ?\C-7 ?\C-6 delete ?\C-7 ?\C-6 kp-subtract home])
(global-set-key (kbd "C-c '") 'header-function)
;; macro for function
(fset 'delete-space-before-function-parenthese
  [home ?\C-s ?\( left backspace home])
(global-set-key (kbd "C-c ;") 'delete-space-before-function-parenthese)


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; custom grep for NBNF LL project
    (global-set-key     (kbd "C-c n n")         'nll-grep-find)
    ;; custom grep for NBNF HL project
    (global-set-key     (kbd "C-c n h")         'nhl-grep-find)
    ;; custom grep for NSF project
    (global-set-key     (kbd "C-c n s")         'nsf-grep-find)
    ;; custom grep for ENSF project
    (global-set-key     (kbd "C-c n e")         'ecar-grep-find)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; preprocess a C macro for NSF project
    (global-set-key     (kbd "C-c m s")         'nsf-c-expand-macro)
    ;; preprocess a C macro for NBNF HL project
    (global-set-key     (kbd "C-c m h")         'nhl-c-expand-macro)
    ;; preprocess a C macro for NBNF LL project
    (global-set-key     (kbd "C-c m n")         'nll-c-expand-macro)
    ;; preprocess a C macro for ENSF project
    (global-set-key     (kbd "C-c m e")         'ecar-c-expand-macro)
    ;; preprocess a C macro for XL1 project
    (global-set-key     (kbd "C-c m x")         'xl1-c-expand-macro)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-12-magneti-marelli)

;;; 01-function-12-magneti-marelli.el ends here
