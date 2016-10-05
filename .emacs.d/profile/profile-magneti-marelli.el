;;; profile-magneti-marelli.el --- a config file for profile

;; Copyright (c) 2012-2016 Claude Tete
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
(when section-environment
  (setq section-environment-profile nil)
  (when section-environment-profile
    (setq profile "Magneti Marelli")
    )
  (setq section-environment-version-recognition t)
  (setq section-environment-os-recognition t)
  (setq section-environment-terminal-vs-graphics t)
  (setq section-environment-set-path t)
  (when section-environment-set-path
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
  (when section-environment-executable
    (setq profile-shell-file-name "D:/cygwin/bin/bash.exe")
    (setq profile-ediff-diff-program "d:/cygwin/usr/bin/GnuWin32/bin/diff.exe")
    (setq profile-ediff-diff3-program "d:/cygwin/usr/bin/GnuWin32/bin/diff3.exe")
    (setq profile-ediff-cmp-program "d:/cygwin/usr/bin/GnuWin32/bin/cmp.exe")
    )
  (setq section-environment-elpa nil)
  (when section-environment-elpa
    (setq profile-environment-elpa-proxy-http nil)
    (setq profile-environment-elpa-proxy-https nil)
    (setq profile-environment-elpa-package-list '())
    )
  (setq section-environment-hyper t)
  (setq section-environment-super nil)
  (setq section-environment-server nil)
  )

(setq section-functions t)
(when section-functions
  (setq section-function-mm t)
  )

(setq section-mode t)
(when section-mode
  (setq section-mode-directory t)
  (setq section-mode-vectra nil)
  (setq section-mode-home-end t)
  (setq section-mode-doxymacs nil)
  (setq section-mode-ido nil)
  (setq section-mode-uniquify t)
  (setq section-mode-cedet t)
  (when section-mode-cedet
    (setq profile-cedet-path (concat dotemacs-path "/plugins/cedet-1.1/common/cedet.elc"))
    (setq profile-gnu-global (concat dotemacs-path "/plugins/gnu_global_628wb/bin/global.exe"))
    (setq profile-gnu-global-gtags (concat dotemacs-path "/plugins/gnu_global_628wb/bin/gtags.exe"))
    (setq section-mode-cedet-semantic t)
    (when section-mode-cedet-semantic
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
    (when section-mode-cedet-ecb
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
            ("^\\.\\(emacs\\|gnus\\)$")
          ))
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
  (when section-mode-c
    (setq section-mode-c-cwarn nil)
    (setq section-mode-c-data-debug nil)
    )
  (setq section-mode-icompletion nil)
  (setq section-mode-yasnippet nil)
  (setq section-mode-browse-kill-ring t)
  (setq section-mode-mm t)
  (when section-mode-mm
    (setq section-mode-mm-eol t)
    (setq section-mode-mm-dbc t)
    (setq section-mode-mm-diff t)
    )
  (setq section-mode-dired-plus t)
  (setq section-mode-gnu-global t)
  (when section-mode-gnu-global
    (setq section-mode-gnu-global-gtags t)
    (setq section-mode-gnu-global-ggtags nil)
    )
  (setq section-mode-eproject nil)
  (setq section-mode-rtrt-script nil)
  (setq section-mode-vc-clearcase nil)
  (when section-mode-vc-clearcase
    (setq profile-clearcase-vtree "c:/path/to/ClearCase/bin/clearvtree.exe")
    (setq profile-cleartool "c:/path/to/ClearCase/bin/cleartool.exe")
    )
  (setq section-mode-clearcase nil)
  (when section-mode-clearcase
    (setq section-mode-clearcase-el nil)
    )
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
  (when section-mode-ack
    (setq section-mode-ack-full nil)
    (setq section-mode-ack-and-half nil)
    (setq section-mode-ack-emacs t)
    )
  (setq section-mode-ace-jump t)
  (setq section-mode-diredful t)
  (setq section-mode-ps2pdf t)
  (setq section-mode-auctex nil)
  (setq section-mode-helm nil)
  (when section-mode-helm
    (setq section-mode-helm-bookmark t)
    (setq section-mode-helm-buffers-list t)
    (setq section-mode-helm-kill-ring t)
    (setq section-mode-helm-M-x nil)
    (setq section-mode-helm-occur t)
    (setq section-mode-helm-find-files t)
    (setq section-mode-helm-recentf t)
    (setq section-mode-helm-imenu t)
    )
  (setq section-mode-yascroll nil)
  (when section-mode-yascroll
    (setq profile-yascroll-delay-to-hide nil)
  (setq section-mode-smart-forward nil)
  (setq section-mode-rainbow nil)
  (setq section-mode-ediff nil)
  (setq section-mode-magit nil)
  (when section-mode-magit
    (setq profile-magit-exec "git")
    )
  (setq section-mode-synergy nil)
  (when section-mode-synergy
    (setq profile-synergy-username "")
    (setq profile-synergy-database "")
    (setq profile-synergy-server "")
    (setq profile-synergy-history-filter nil)
    (setq profile-synergy-diff-external-command nil)
    (setq profile-synergy-diff-external-parameter nil)
    (setq profile-synergy-diff-external-swap-file nil)
    ) ; (when section-mode-synergy
  (setq section-mode-hide-lines nil)
  (setq section-mode-aggressive-indent nil)
  (setq section-mode-platinium-search nil)
  (setq section-mode-popwin nil)
  (setq section-mode-projectile nil)
  (setq section-mode-company nil)
  (setq section-mode-expand-region nil)
  (setq section-mode-function-args nil)
  (setq section-mode-elpy nil)
  (when section-mode-elpy
    ;; add elpy package
    ;; and flycheck package, about warnings/errors check on the fly
    (add-to-list 'profile-environment-elpa-package-list 'elpy t)
    (add-to-list 'profile-environment-elpa-package-list 'flycheck t)
    ) ; (when section-mode-elpy
  (setq section-mode-diminish nil)
  )

(setq section-languages t)
(when section-languages
  (setq section-languages-c t)
  (when section-languages-c
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
    (when section-languages-c-hide-show
      (setq section-languages-c-hide-show-hide-all-at-start nil)
      )
    )
    (setq section-languages-c-flymake nil)
  (setq section-languages-lisp t)
  (when section-languages-lisp
    (setq profile-lisp-indent-offset 2)
    )
  (setq section-languages-tabulation t)
  (setq section-languages-rtrt-script nil)
  (setq section-languages-perl t)
  (when section-languages-perl
    (setq profile-perl-indent-offset 2)
    )
  (setq section-languages-c++-qt nil)
  )

(setq section-selection t)
(when section-selection
  (setq section-selection-with-shift nil)
  )

(setq section-display t)
(when section-display
  (setq section-display-windows-buffers t)
  (when section-display-windows-buffers
    (setq section-display-windows-buffers-visual-line nil)
    )
  (setq section-display-speedbar nil)
  (setq section-display-font t)
  (when section-display-font
    (setq section-display-font-antialias nil)
    (setq profile-font "Terminal-6")
    (setq section-display-font-international t)
    )
  (setq section-display-color t)
  (when section-display-color
    (setq section-display-color-parentheses-mode t)
    (setq section-display-color-parentheses-visible t)
    (setq section-display-color-parentheses-highlight nil)
    (setq section-display-color-theme t)
    (if section-display-color-theme
      (setq profile-color-theme "sweet")
    (setq section-display-color-ansi-color-compile t)
    (setq section-display-color-highlight-line nil)
      )
    )
  )

(setq section-interface t)
(when section-interface
  (setq section-interface-remove-decoration t)
  (setq section-interface-modeline t)
  (setq profile-window-title "<[ %b ]>")
  (setq section-interface-transparency t)
  (when section-interface-transparency
    (setq profile-transparency 90)
    )
  (setq section-interface-fullscreen t)
  (setq section-interface-ecb t)
  (when section-interface-ecb
    (setq section-interface-ecb-ascii-tree t)
    )
  )

(setq section-completion t)

(setq section-shortcut t)
(when section-shortcut
  (setq section-shortcut-global t)
  (when section-shortcut-global
    (setq section-shortcut-global-cua nil)
    )
  (setq section-shortcut-windows t)
  (setq section-shortcut-buffers t)
  (setq section-shortcut-ecb t)
  (setq section-shortcut-grep t)
  (setq section-shortcut-function t)
  (setq section-shortcut-tags t)
  (when section-shortcut-tags
    (setq section-shortcut-tags-exuberant-ctags nil)
    (setq section-shortcut-tags-gnu-global t)
    )
  (setq section-shortcut-semantic t)
  )

(setq section-mouse t)
(when section-mouse
  (setq section-mouse-paste-to-point-not-mouse-cursor t)
  (setq section-mouse-avoidance t)
  (setq section-mouse-smooth-scroll t)
)

(setq section-annoyances t)
(when section-annoyances
  (setq section-annoyances-truncate-line nil)
  (setq section-annoyances-scroll-preserve-cursor-position nil)
  (setq section-annoyances-no-backup-file nil)
  (setq section-annoyances-backup-file-in-directory t)
  )

(setq section-misc t)
(when section-misc
  (setq profile-remove-useless-ending-space t)
  (setq profile-always-new-line-at-end t)
  (setq profile-backup-directory (concat dotemacs-path "/backup"))
  (setq profile-backup-directory (concat dotemacs-path "/cache"))
  (setq profile-fill-column 78)
  (setq profile-browser "d:/cygwin/usr/bin/OperaPortable/Opera.exe")
  (setq section-misc-calendar nil)
  (when section-misc-calendar
    (setq section-misc-calendar-french nil)
    )
  (setq section-misc-dictionary nil)
  (when section-misc-dictionary
    (setq profile-ispell-program "aspell")
    (setq profile-ispell-dictionary "english")
    )
  (setq section-misc-bookmark t)
  (when section-misc-bookmark
    (setq profile-bookmark-sort nil)
    )
  (setq section-misc-screensaver nil)
  )

(setq section-filecustomize t)

(defun function-to-call-after-loading-conf ()
  ) ; (defun function-to-call-after-loading-conf ()


(provide 'profile-magneti-marelli)

;;; profile-magneti-marelli.el ends here
