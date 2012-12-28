;;; profile-magneti-marelli.el --- a config file for profile

;; Copyright (c) 2012 Claude Tete
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

;;; Commentary:
;;
;; load by `dotemacs/profile.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment-profile'


;;; Code:
;; load private variable
(try-require 'profile-magneti-marelli-private "    ")

;; Enable/disable section:
;; (setq section-xxx X) where 'xxx' means a section of .emacs and 'X' take
;; 'nil' (disable) or 't' (enable) value
;;
;; Settings:
;; (setq profile-xxx X)

(setq section-environment t)
(progn
  (setq section-environment-profile nil)
  (progn
    (setq profile "Magneti Marelli")
    )
  (setq section-environment-version-recognition t)
  (setq section-environment-os-recognition t)
  (setq section-environment-terminal-vs-graphics t)
  (setq section-environment-set-path t)
  (progn
    (setq profile-path
      (concat
        "d:/cygwin/bin" ";"
        "d:/cygwin/usr/bin/gnuwin32/bin" ";"
        )
      )
    (setq profile-exec-path
      '(
         "d:/cygwin/bin"
         "d:/cygwin/usr/bin/gnuwin32/bin"
         )
      )
    (setq profile-lang "en_US")
    )
  (setq section-environment-ms-windows-performance t)
  (setq section-environment-executable t)
  (progn
    (setq profile-shell-file-name "D:/cygwin/bin/bash.exe")
    (setq profile-ediff-diff-program "d:/cygwin/usr/bin/GnuWin32/bin/diff.exe")
    (setq profile-ediff-diff3-program "d:/cygwin/usr/bin/GnuWin32/bin/diff3.exe")
    (setq profile-ediff-cmp-program "d:/cygwin/usr/bin/GnuWin32/bin/cmp.exe")
    )
  (setq section-environment-elpa nil)
  (setq section-environment-hyper t)
  (setq section-environment-super nil)
  )

(setq section-functions t)
(progn
  (setq section-function-mm t)
  )

(setq section-mode t)
(progn
  (setq section-mode-directory t)
  (setq section-mode-vectra nil)
  (setq section-mode-home-end t)
  (setq section-mode-doxymacs nil)
  (setq section-mode-ido nil)
  (setq section-mode-uniquify t)
  (setq section-mode-cedet t)
  (progn
    (setq profile-cedet-path (concat dotemacs-path "/plugins/cedet-1.1/common/cedet.elc"))
    (setq profile-gnu-global (concat dotemacs-path "/plugins/gnu_global_622wb/bin/global.exe"))
    (setq profile-gnu-global-gtags (concat dotemacs-path "/plugins/gnu_global_622wb/bin/gtags.exe"))
    (setq section-mode-cedet-semantic t)
    (progn
      (setq profile-ede-project
        '(
           "d:/ccm_wa/XL1/XL1.ede.el"
           "d:/ccm_wa/NSF/NSF/NSF_CLIENT.ede.el"
           "d:/ccm_wa/NBNF/NBNF_LL/NBNFLL.ede.el"
           "d:/ccm_wa/NBNF/NBNF_HL/NBNF_HL.ede.el"
           "d:/ccm_wa/ECAR/ENSF.ede.el"
           "d:/ccm_wa/PQ36/PQ36_HL/PQ35GPHL.ede.el"
           )
        )
      )
    (setq section-mode-cedet-ecb t)
    (progn
      (setq profile-ecb-source-path
        '(
           ("c:/Documents and Settings/tete"                     "/tete")
           ("h:/"                                                "/home")
           ("d:/cygwin/usr/bin"                                  "/bin")
           ("d:/cygwin/usr/bin/.emacs.d"                         "/emacs.d")
           ("c:/Documents and Settings/tete/Local Settings/Temp" "/temp")
           )
        )
      (setq profile-ecb-excluded-directories-regexps
        '(
           "^\\(CVS\\|\\.[^xX]*\\)$"
           "^\\(Doc\\|Dev\\|.*_root\\|Application\\|Base\\|Control\\|System\\|Presentation\\|TechnicalNote\\|HTML\\)$"
           "\\(01_ProjectPlan\\|02_ProjectMonitoringAndControl\\|03_RequirementManagement\\|04_SupplierAgreementManagement\\|05_ProcessAndProductQualityAssurance\\|06_ConfigurationManagement\\|07_RequirementDevelopment\\|08_TechnicalSolution\\|10_Verification\\|11_Validation\\)$"
           "\\(01_SystemIntegrationAndValidationPlan\\|02_SystemIntegrationTestCases\\|03_SystemIntegrationTestReport\\|05_DeliveryToCustomer\\|06_DeliveryToInternal\\|88_VerificationReportsAndChecklists\\|99_OtherPI\\)$"
           )
        )
      (setq profile-ecb-source-file-regexps
        '((".*"
            ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|_ccmwaid\\.inf\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|xls\\|doc\\)$\\)\\)")
            ("^\\.\\(emacs\\|gnus\\)$"))
          )
        )
      (setq profile-ecb-sources-exclude-cvsignore
        '(
           "_ccmwaid.inf"
           )
        )
      (setq profile-ecb-history-make-buckets
        '(
           "|ECAR"
           "|NBNF_LL"
           "|NBNF_HL"
           "|NSF"
           "|XL1"
           "|PQ35GPHL"
           "\\.el$"
           )
        )
      )
    )
  (setq section-mode-batch t)
  (setq section-mode-vb t)
  (setq section-mode-window-numbering nil)
  (setq section-mode-c t)
  (progn
    (setq section-mode-c-cwarn nil)
    (setq section-mode-c-data-debug nil)
    )
  (setq section-mode-icompletion nil)
  (setq section-mode-yasnippet nil)
  (setq section-mode-browse-kill-ring t)
  (setq section-mode-mm t)
  (progn
    (setq section-mode-mm-eol t)
    (setq section-mode-mm-dbc t)
    (setq section-mode-mm-diff t)
    )
  (setq section-mode-dired-plus t)
  (setq section-mode-gnu-global t)
  (setq section-mode-eproject nil)
  (setq section-mode-rtrt-script nil)
  (setq section-mode-vc-clearcase nil)
  (progn
    (setq profile-clearcase-vtree "c:/path/to/ClearCase/bin/clearvtree.exe")
    (setq profile-cleartool "c:/path/to/ClearCase/bin/cleartool.exe")
    )
  (setq section-mode-clearcase nil)
  (setq section-mode-autohotkey nil)
  (setq section-mode-outline t)
  (setq section-mode-auto-highlight-symbol t)
  (setq section-mode-google-calendar nil)
  (setq section-mode-fill-column-indicator nil)
  (setq section-mode-muse nil)
  (setq section-mode-undo-tree t)
  (setq section-mode-csv nil)
  (setq section-mode-subversion nil)
  (setq section-mode-diff-color t)
  (setq section-mode-dired-sort t)
  (setq section-mode-org-mode nil)
  (setq section-mode-isearch+ nil)
  (setq section-mode-psvn nil)
  (setq section-mode-powerline nil)
  (progn
    (setq profile-powerline-size "big")
    )
  (setq section-mode-nyan nil)
  (setq section-mode-sml nil)
  (setq section-mode-dired t)
  (setq section-mode-isearch t)
  (setq section-mode-rainbow-delimiters nil)
  (setq section-mode-calfw nil)
  (setq section-mode-dired-details nil)
  (setq section-mode-smart-tab nil)
  (setq section-mode-fold-dwim t)
  (setq section-mode-dired-lis nil)
  (setq section-mode-nxhtml nil)
  (setq section-mode-fastnav nil)
  (setq section-mode-mru-yank t)
  (setq section-mode-ack t)
  (progn
    (setq section-mode-ack-full nil)
    (setq section-mode-ack-and-half nil)
    (setq section-mode-ack-emacs t)
    )
  (setq section-mode-ace-jump t)
  (setq section-mode-diredful t)
  (setq section-mode-ps2pdf t)
  (setq section-mode-diminish nil)
  )

(setq section-languages t)
(progn
  (setq section-languages-c t)
  (progn
    (setq profile-c-indent-offset 2)
    (setq profile-c-extra-types
      '(
         "ubyte"
         "ushort"
         "ulong"
         "ulonglong"
         "sbyte"
         "sshort"
         "slong"
         "slonglong"
         )
      )
    (setq profile-c-macro-preprocessor "cpp -C")
    (setq profile-c-macro-cppflags "-D__ALONE_MICRO__ -D__CLIENT_EOL_LINK__ -D__EOL_ENABLE__ -D__RTOS__")
    (setq profile-c-ask-before-compile t)
    (setq section-languages-c-indent-preprocessor t)
    (setq section-languages-c-hide-show t)
    (progn
      (setq section-languages-c-hide-show-hide-all-at-start nil)
      )
    )
  (setq section-languages-lisp t)
  (progn
    (setq profile-lisp-indent-offset 2)
    )
  (setq section-languages-tabulation t)
  (setq section-languages-rtrt-script nil)
  (setq section-languages-perl t)
  (progn
    (setq profile-perl-indent-offset 2)
    )
  )

(setq section-selection t)
(progn
  (setq section-selection-with-shift nil)
  )

(setq section-display t)
(progn
  (setq section-display-windows-buffers t)
  (progn
    (setq section-display-windows-buffers-visual-line nil)
    )
  (setq section-display-speedbar nil)
  (setq section-display-font t)
  (progn
    (setq profile-font "Terminal-6")
    (setq section-display-font-international t)
    )
  (setq section-display-color t)
  (progn
    (setq section-display-color-parentheses-mode t)
    (setq section-display-color-parentheses-visible t)
    (setq section-display-color-parentheses-highlight nil)
    (setq section-display-color-theme t)
    (progn
      (setq profile-color-theme "sweet")
      (setq section-display-color-misc nil)
      (setq section-display-color-mode nil)
      (setq section-display-color-grep nil)
      (setq section-display-color-ecb nil)
      )
    )
  )

(setq section-interface t)
(progn
  (setq section-interface-remove-decoration t)
  (setq section-interface-modeline t)
  (setq section-interface-transparency t)
  (progn
    (setq profile-transparency 90)
    )
  (setq section-interface-fullscreen t)
  (setq section-interface-ecb t)
  (progn
    (setq section-interface-ecb-ascii-tree t)
    )
  )

(setq section-completion t)

(setq section-shortcut t)
(progn
  (setq section-shortcut-global t)
  (progn
    (setq section-shortcut-global-cua nil)
    )
  (setq section-shortcut-windows t)
  (setq section-shortcut-buffers t)
  (setq section-shortcut-ecb t)
  (setq section-shortcut-grep t)
  (setq section-shortcut-function t)
  (setq section-shortcut-tags t)
  (progn
    (setq section-shortcut-tags-exuberant-ctags nil)
    (setq section-shortcut-tags-gnu-global t)
    )
  (setq section-shortcut-semantic t)
  )

(setq section-mouse t)
(progn
  (setq section-mouse-paste-to-point-not-mouse-cursor t)
  (setq section-mouse-avoidance t)
)

(setq section-annoyances t)
(progn
  (setq section-annoyances-truncate-line nil)
  (setq section-annoyances-scroll-preserve-cursor-position nil)
  (setq section-annoyances-no-backup-file nil)
  (setq section-annoyances-backup-file-in-directory t)
  (setq section-annoyances-classic-scroll nil)
  )

(setq section-misc t)
(progn
  (setq profile-backup-directory (concat dotemacs-path "/backup"))
  (setq profile-backup-directory (concat dotemacs-path "/cache"))
  (setq profile-fill-column 78)
  (setq profile-browser "d:/cygwin/usr/bin/OperaPortable/Opera.exe")
  (setq section-misc-calendar nil)
  (progn
    (setq section-misc-calendar-french nil)
    )
  (setq section-misc-dictionary nil)
  (progn
    (setq profile-ispell-program "aspell")
    (setq profile-ispell-dictionary "english")
    )
  (setq section-misc-bookmark t)
  (setq section-misc-screensaver nil)
  )

(setq section-filecustomize t)

(defun function-to-call-after-loading-conf ()
  ) ; (defun function-to-call-after-loading-conf ()


(provide 'profile-magneti-marelli)

;;; profile-magneti-marelli.el ends here
