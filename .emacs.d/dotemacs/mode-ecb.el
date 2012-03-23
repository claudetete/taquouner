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
;; Version: 1.4
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
(cond
  ;; Magneti Marelli -----------------------------------------------------------
  ((string= clt-working-environment "Magneti Marelli")
    (custom-set-variables
      '(cedet-global-command "d:/cygwin/bin/global.exe")
      '(cedet-global-gtags-command "d:/cygwin/bin/gtags.exe"))
    ) ; Magneti Marelli

  ;; Alstom Transport ----------------------------------------------------------
  ((string= clt-working-environment "Alstom Transport")
    (custom-set-variables
      '(cedet-global-command "d:/cygwin/usr/local/bin/global.exe")
      '(cedet-global-gtags-command "d:/cygwin/usr/local/bin/gtags.exe"))
    ) ; Alstom Transport

  ) ; cond ---------------------------------------------------------------------

;; donne le chemin d'installation d'ecb
(add-to-list 'load-path (concat dotemacs-path "/plugins/ecb-snap"))

;;
;;; ECB
(when (try-require 'ecb)
  ;; load all necessary for ecb
  (require 'ecb-autoloads)

  (custom-set-variables
    '(ecb-auto-activate t)

    ;; donne la version /* pas tres utile sauf pour check install */
    '(ecb-options-version "2.40")
    )

  ;; les chemins par defaut dans "ecb directories" (repertoires)
  (cond
    ;; Magneti Marelli-----------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (custom-set-variables
        '(ecb-source-path
           (quote
             (
               ;; here is put the ede project (see project.el)
               ("c:/Documents and Settings/tete"            "/tete")
               ("h:/"                                       "/home")
               ("d:/cygwin/usr/bin"                         "/bin")
               ("d:/cygwin/usr/bin/.emacs.d"                "/emacs.d")
               ("c:/Documents and Settings/tete/Local Settings/Temp" "/temp")
               )))

        ;; regex des dossiers a exclure de la window directory
        '(ecb-excluded-directories-regexps
           (quote (
                    "^\\(CVS\\|\\.[^xX]*\\)$"
                    "^\\(Doc\\|Dev\\|.*_root\\|Application\\|Base\\|Control\\|System\\|Presentation\\|TechnicalNote\\|HTML\\)$"
                    "\\(01_ProjectPlan\\|02_ProjectMonitoringAndControl\\|03_RequirementManagement\\|04_SupplierAgreementManagement\\|05_ProcessAndProductQualityAssurance\\|06_ConfigurationManagement\\|07_RequirementDevelopment\\|08_TechnicalSolution\\|10_Verification\\|11_Validation\\)$"
                    "\\(01_SystemIntegrationAndValidationPlan\\|02_SystemIntegrationTestCases\\|03_SystemIntegrationTestReport\\|05_DeliveryToCustomer\\|06_DeliveryToInternal\\|88_VerificationReportsAndChecklists\\|99_OtherPI\\)$")))

        ;; fichiers a ignorer dans les source !! lire la doc !!
        '(ecb-source-file-regexps
           (quote ((".*"
                     ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|_ccmwaid\\.inf\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|xls\\|doc\\)$\\)\\)")
                     ("^\\.\\(emacs\\|gnus\\)$")))))

        ;; fichiers a ignorer comme Version Control VC
        '(ecb-sources-exclude-cvsignore (quote ("_ccmwaid.inf")))

        ;; regexp pour regrouper dans ecb-history
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
        '(ecb-source-path
           (quote
             (
               ;; here is put the ede project (see project.el)
               ("m:/e_ctete/a2kc/test/CCN4/test_s/pm4s/RTRT/"          ".../test_s/pm4s/RTRT")
               ;;("m:/e_ctete"                                           "/myClearCase")
               ("d:/Documents and Settings/100516805/Application Data" "/home")
               ("m:/"                                                  "/ClearCase")
               ("d:/Users/ctete/tools"                                 "/tools")
               ("d:/Users/ctete/tmp"                                   "/tmp")
               )))

        ;; regex des dossiers a exclure de la window directory
        '(ecb-excluded-directories-regexps
           (quote (
                    ;;"^\\(CVS\\|\\.[^xX]*\\)$"
                    "^\\.+$"
                    "^\\(TOTO\\|TITI\\)$"
                    "\\(Cvisualdspplus2\\|RTRT_res\\)$" ; RTRT
                    "\\(TOTO\\|TITI\\)$")))             ; example


        ;; fichiers a ignorer dans les source !! lire la doc !!
        '(ecb-source-file-regexps
           (quote ((".*"
                     ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|ri2\\|fdc\\|lis\\|a\\|so\\|tcl\\|summary\\.txt\\|atc\\.txt\\)$\\)\\)")
                     ("^\\.\\(emacs\\|gnus\\)$")))))

        ;;;; fichiers a ignorer comme Version Control VC
        ;;'(ecb-sources-exclude-cvsignore (quote ("_ccmwaid.inf")))

        ;; regexp pour regrouper dans ecb-history
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

  ;;;; demande a chaque fois qu'un buffer est fermer si il doit etre supprimer
  ;;;; de la liste ecb-history
  ;;'(ecb-kill-buffer-clears-history (quote ask))

  ;; comment afficher les fonctions dans "ecb methods"
  '(ecb-show-tags
     (quote ((default
               (include collapsed nil)
               (parent collapsed nil)
               (type flattened nil)
               (variable collapsed access)
               (function flattened nil)
               (label hidden nil)
               (t collapsed nil))

              ;; mode C (collapsed = arborescence ; flattened = a plat
              ;; nil = pas de tri)
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

  ;; aucune idee
  '(ecb-type-tag-expansion
     (quote ((default
               "class"
               "interface"
               "group"
               "namespace")
              (c-mode))
       ))

  ;; desactive la gestion des systemes de controle de version (VC)
  '(ecb-vc-enable-support nil)

  ;;;; donne la fonction courrante dans la barre d'etat
  ;;'(which-function-mode t)

  ;; ajoute buffers que je veux afficher dans la window de compilation
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
  ;; permet de modifier le fichier TAGS lors de l'ouverture de fichier
  '(tags-loop-revert-buffers t)
  ;;
  ;; couleur des lignes trouver dans le buffer *Tag List*
  '(tags-tag-face (quote match)))

;; permet a ecb de prendre la main lors de l'ouverture de la fenetre de
;; compilation/grep/...
(add-hook 'ecb-activate-hook
  (lambda ()
    (let ((compwin-buffer (ecb-get-compile-window-buffer)))
      (if (not (and compwin-buffer
                 (ecb-compilation-buffer-p compwin-buffer)))
        (ecb-toggle-compile-window -1)))))

) ; (when (try-require 'ecb)

;;; mode-ecb.el ends here
