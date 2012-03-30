;;; mode-ecb.el --- a config file for ecb mode settings

;; Copyright (c) 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, ecb, mode, ide
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.5
;; Created: August 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `mode.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-mode-cedet-ecb'
;;              var     `section-mode-cedet'
;;
;; INSTALL
;; ne pas oublier de lancer cedet (pas le plus sinon ca bug) avant et
;; une fois dans Emacs d'activer avec ecb-byte-compile pour finir :)
;;
;; LOAD & SETTING
;; pour lancer tous les jours faire 'ecb-activate' une fois Emacs demarrer

;;; Change Log:
;; 2012-03-29 (1.5)
;;    translate comments in english + add some fix
;; 2012-03-05 (1.4)
;;    remove fix fullscreen + add some settings
;; 2012-03-02 (1.3)
;;    fix fullscreen in win32
;; 2011-07-09 (1.2)
;;    arrange file remove all dead code etc...
;; 2011-04-21 (1.1)
;;    add test about ms windows for path
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2010-08-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; path of ecb
(add-to-list 'load-path (concat dotemacs-path "/plugins/ecb-snap"))

;;
;;; ECB
(when (try-require 'ecb "      ")
  ;; load all necessary for ecb
  (require 'ecb-autoloads)

  (custom-set-variables
    '(ecb-auto-activate t)

    ;; set version (only for check install)
    '(ecb-options-version "2.40")
    )

  (cond
    ;; Magneti Marelli-----------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (custom-set-variables
        ;; set default path in "ecb directories"
        '(ecb-source-path
           (quote
             (
               ;; before is put the ede projects (see project.el)
               ("c:/Documents and Settings/tete"            "/tete")
               ("h:/"                                       "/home")
               ("d:/cygwin/usr/bin"                         "/bin")
               ("d:/cygwin/usr/bin/.emacs.d"                "/emacs.d")
               ("c:/Documents and Settings/tete/Local Settings/Temp" "/temp")
               )))

        ;; regexp of folder to exclude in "ecb directories"
        '(ecb-excluded-directories-regexps
           (quote (
                    "^\\(CVS\\|\\.[^xX]*\\)$"
                    "^\\(Doc\\|Dev\\|.*_root\\|Application\\|Base\\|Control\\|System\\|Presentation\\|TechnicalNote\\|HTML\\)$"
                    "\\(01_ProjectPlan\\|02_ProjectMonitoringAndControl\\|03_RequirementManagement\\|04_SupplierAgreementManagement\\|05_ProcessAndProductQualityAssurance\\|06_ConfigurationManagement\\|07_RequirementDevelopment\\|08_TechnicalSolution\\|10_Verification\\|11_Validation\\)$"
                    "\\(01_SystemIntegrationAndValidationPlan\\|02_SystemIntegrationTestCases\\|03_SystemIntegrationTestReport\\|05_DeliveryToCustomer\\|06_DeliveryToInternal\\|88_VerificationReportsAndChecklists\\|99_OtherPI\\)$")))

        ;; files to be ignored in "ecb source" !! RTFM !!
        '(ecb-source-file-regexps
           (quote ((".*"
                     ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|_ccmwaid\\.inf\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|xls\\|doc\\)$\\)\\)")
                     ("^\\.\\(emacs\\|gnus\\)$")))))

        ;; files to be ignored from Version Control VC
        '(ecb-sources-exclude-cvsignore (quote ("_ccmwaid.inf")))

        ;; regexp to form group in "ecb history"
        '(ecb-history-make-buckets
           (quote (
                    "|ECAR"
                    "|NBNF_LL"
                    "|NBNF_HL"
                    "|NSF"
                    "|XL1"
                    "|PQ35GPHL"
                    "\\.el$")))
        )
      ) ; Magneti Marelli

    ;; Alstom Transport------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (custom-set-variables
        ;; set default path in "ecb directories"
        '(ecb-source-path
           (quote
             (
               ;; before is put the ede projects (see project.el)
               ("m:/e_ctete/a2kc/test/CCN4/test_s/pm4s/RTRT/"          ".../test_s/pm4s/RTRT")
               ("d:/Documents and Settings/100516805/Application Data" "/home")
               ("m:/"                                                  "/ClearCase")
               ("d:/Users/ctete/tools"                                 "/tools")
               ("d:/Users/ctete/tmp"                                   "/tmp")
               )))

        ;; regexp of folder to exclude in "ecb directories"
        '(ecb-excluded-directories-regexps
           (quote (
                    "^\\.+$"
                    "^\\(TOTO\\|TITI\\)$"
                    "\\(Cvisualdspplus2\\|RTRT_res\\)$" ; RTRT
                    "\\(TOTO\\|TITI\\)$")))             ; example


        ;; files to be ignored in "ecb source" !! RTFM !!
        '(ecb-source-file-regexps
           (quote ((".*"
                     ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|ri2\\|fdc\\|map\\|lis\\|a\\|so\\|tcl\\|summary\\.txt\\|atc\\.txt\\)$\\)\\)")
                     ("^\\.\\(emacs\\|gnus\\)$")))))

        ;;;; files to be ignored from Version Control VC
        ;;'(ecb-sources-exclude-cvsignore (quote ("_ccmwaid.inf")))

        ;; regexp to form group in "ecb history"
        '(ecb-history-make-buckets
           (quote (
                    "ccn4_pm4s"
                    "include"
                    "\\.ptu$"
                    "\\.ahk$"
                    "\\.[hc][p]*$"
                    "\\.el$")))
        )
      ) ; Alstom Transport

    ) ; cond ------------------------------------------------------------

(custom-set-variables

  ;;;; ask each time I kill a buffer if I want keep it in "ecb history"
  ;;;; a pain to ask each time...
  ;;'(ecb-kill-buffer-clears-history (quote ask))

  ;; how to show functions in "ecb methods"
  '(ecb-show-tags
     (quote ((default
               (include collapsed nil)
               (parent collapsed nil)
               (type flattened nil)
               (variable collapsed access)
               (function flattened nil)
               (label hidden nil)
               (t collapsed nil))

              ;; mode C (collapsed = tree ; flattened = flat
              ;; nil = no order)
              (c-mode
                (include collapsed nil)
                (parent collapsed nil)
                (type flattened nil)
                (variable collapsed nil)
                (function flattened nil)
                (function collapsed nil)
                (label hidden nil)
                (t collapsed nil))
              )))

  ;; no idea??
  '(ecb-type-tag-expansion
     (quote ((default
               "class"
               "interface"
               "group"
               "namespace")
              (c-mode))
       ))

  ;; disable Version Control for ecb
  '(ecb-vc-enable-support nil)

  ;;;; show current function inf modeline
  ;;;; see semantic
  ;;'(which-function-mode t)

  ;; add all buffers name I want in the compil window of ecb
  '(ecb-compilation-buffer-names
     (quote (
              ("*Calculator*")
              ("*vc*")
              ("*vc-diff*")
              ("*Apropos*")
              ("*Occur*")
              ("*shell*")
              ("\\*[cC]ompilation.*\\*" . t)
              ("\\*i?grep.*\\*" . t)
              ("*JDEE Compile Server*")
              ("*Help*")
              ("*Completions*")
              ("*Backtrace*")
              ("*Compile-log*")
              ("*bsh*")
              ("*Messages*")
              ("msg.txt.*" . t)
              ("*ccm*")
              ("\\*GTAGS SELECT\\*.*" . t)
              ("\\*.*\\*" . t)
              ("*Bookmark List*")
              ("*Shell Command Output*")
              ("*Semantic Context Analyze*")
              ("*Macroexpansion*")
              ("\\*Symref .*" . t)
              ("*ECB Analyse*"))))

  ;; auto expand tree
  '(ecb-auto-expand-tag-tree-collapse-other (quote only-if-on-tag))
  '(ecb-expand-methods-switch-off-auto-expand nil)

  ;; do not prescan the directory for emptyness
  '(ecb-prescan-directories-for-emptyness nil)

  ;; do not process file without semantic
  '(ecb-process-non-semantic-files nil)
  ) ; (custom-variables

(custom-set-variables
  ;; tags file can be modified when a file is opened
  '(tags-loop-revert-buffers t)
  ;;
  ;; color of lines find in *Tag List*
  '(tags-tag-face (quote match)))

;; let ecb take control of opening of compil window
(add-hook 'ecb-activate-hook
  (lambda ()
    (let ((compwin-buffer (ecb-get-compile-window-buffer)))
      (if (not (and compwin-buffer
                 (ecb-compilation-buffer-p compwin-buffer)))
        (ecb-toggle-compile-window -1)))))

) ; (when (try-require 'ecb)

;; The adviced version of switch-to-buffer-other-window can redraw the layout
;; (e.g. if the buffer in the compile-window is the slave and the
;; compile-window has been made visible), so <window> in the code below can be
;; a destroyed window-object! we have to prevent from this (e.g. by selecting
;; the window before by number).
(when-ecb-running-emacs
 (defecb-advice master-says around ecb-compatibility-advices
   "Makes the function compatible with ECB."
   (if (or (not ecb-minor-mode)
           (not (equal (selected-frame) ecb-frame)))
       (ecb-with-original-basic-functions ad-do-it)
     (if (null (buffer-live-p (get-buffer master-of)))
         (error "Slave buffer has disappeared")
       (let ((window  (selected-window))
             (point-loc (ecb-where-is-point))
             (p (point)))
         (if (not (eq (window-buffer window) (get-buffer master-of)))
             (switch-to-buffer-other-window master-of))
         (if (ad-get-arg 0)
             (condition-case nil
                 (apply (ad-get-arg 0) (ad-get-arg 1))
               (error nil)))
         (select-window (case (car point-loc)
                          (ecb
                           (ecb-get-ecb-window-by-number (cdr point-loc)))
                          (edit
                           (ecb-get-edit-window-by-number (cdr point-loc)))
                          (compile
                           ecb-compile-window)
                          (minibuf
                           (minibuffer-window ecb-frame))
                          (other-dedicated
                           (ecb-get-window-by-number (cdr point-loc)))))
         (goto-char (point))))))
   )

;;; mode-ecb.el ends here
