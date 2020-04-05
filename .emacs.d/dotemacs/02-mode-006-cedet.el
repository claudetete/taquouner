;;; 02-mode-006-cedet.el --- configuration of cedet mode

;; Copyright (c) 2006-2020 Claude Tete
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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.2
;; Created: October 2006
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER."Collection of Emacs Development Environment Tools"]
;; [SUBDEFAULT.nil]


;;; Code:


;; only if cedet can be loaded
(use-package cedet
  :config
  ;; [VARCOMMENT.bin path of gnu global for cedet]
  ;; [VARIABLE.tqnr-profile-gnu-global (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_656wb/bin/global.exe")]
  ;; [VARIABLE.tqnr-profile-gnu-global-gtags (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_656wb/bin/gtags.exe")]
  (setq cedet-global-command tqnr-profile-gnu-global)
  (setq cedet-global-gtags-command tqnr-profile-gnu-global-gtags)
  ;; to remove warning:
  ;; Warning: cedet-called-interactively-p called with 0 arguments, but requires 1
  (setq byte-compile-warnings nil)
  ) ;; (use-package cedet


;; [COMMENT.]
;; [VARCOMMENT.CEDET SEMANTIC: can do tag, list of function/variable..., preproc, etc]
;; [VARIABLE.tqnr-section-mode-cedet-semantic t]
(when tqnr-section-mode-cedet-semantic
  (use-package cedet
    :init
    ;; launch semantic mode
    (semantic-mode t)

    ;; run ede mode to manage project files
    (global-ede-mode t)

    :config
    ;; [VARCOMMENT.list of projects: the order is important, display in reverse order (first->last)]
    ;; [[VARIABLE.tqnr-profile-ede-project
    ;;   '(
    ;;      ;; project
    ;;      "c:/path/to/project/project.ede.el"
    ;;      )
    ;; ]]
    (while tqnr-profile-ede-project
      (let (file)
        (setq file (pop tqnr-profile-ede-project))
        (if (file-exists-p file)
          (load-file file)
          (display-warning 'tqnr-profile-ede-project (format "file not found: %s" file) :warning)
          )
        )
      )
    ;; do not use cedet with python (do not work wisent-python-default-setup is still called somewhere)
    (remove-hook 'python-mode-hook 'wisent-python-default-setup)

    ;; Increase the delay before activation
    (setq semantic-idle-scheduler-idle-time 10)
    ;; Don't reparse really big buffers.
    (setq semantic-idle-scheduler-max-buffer-size 100000)

    ;; Increase the delay before doing slow work to 2 minutes.
    (setq semantic-idle-scheduler-work-idle-time 120)

    ;; Minor mode for displaying parser cache state in the modeline.
    (setq semantic-show-parser-state-mode t)

    :custom
    ;; SEMANTIC SETTING
    ;; choose the semanticdb at submode
    (semantic-default-submodes
      '(
         global-semantic-highlight-func-mode
         global-semantic-decoration-mode
         global-semantic-stickyfunc-mode
         global-semantic-idle-scheduler-mode
         global-semanticdb-minor-mode))

    ;; do not use image with semantic
    (semantic-format-use-images-flag nil)

    ;; remove python mode in this list (to work with elpy mode)
    (semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
         (c++-mode . semantic-default-c-setup)
         (html-mode . semantic-default-html-setup)
         (java-mode . wisent-java-default-setup)
         (js-mode . wisent-javascript-setup-parser)
         ;;(python-mode . wisent-python-default-setup)
         (scheme-mode . semantic-default-scheme-setup)
         (srecode-template-mode . srecode-template-setup-parser)
         (texinfo-mode . semantic-default-texi-setup)
         (makefile-automake-mode . semantic-default-make-setup)
         (makefile-gmake-mode . semantic-default-make-setup)
         (makefile-makepp-mode . semantic-default-make-setup)
         (makefile-bsdmake-mode . semantic-default-make-setup)
         (makefile-imake-mode . semantic-default-make-setup)
         (makefile-mode . semantic-default-make-setup)))

    :bind
    ;; go to the tag with shift + left click
    ("<S-down-mouse-1>" . ignore)
    ("<S-mouse-1>" . (lambda (event)
                       ;; the function get a event parameter (from mouse click)
                       (interactive "e")
                       ;; set the point at the mouse position
                       (mouse-set-point event)
                       (semantic-goto-definition (point))))

    ;; right click will open menu with list of variable/function/include
    ("<mouse-3>" . imenu)

    ) ;; (use-package cedet


  (when (not tqnr-section-mode-helm)
    (use-package cedet
      :bind
      ;; go to the tag
      ("M-." . semantic-goto-definition)

      ;; find all references of a symbol by regexp
      ("C-M-." . semantic-symref-regexp)

      ;; return back after "go to the tag"
      ;; you can use the default shortcut: M-xB, which ask where you want go back
      ("C-<" . semantic-pop-tag-mark)
      ("<mouse-4>" . semantic-pop-tag-mark))
    ) ; (when (not tqnr-section-mode-helm)

  ;; semanticdb is used
  (use-package semantic/db
    ;; make sure it is loaded and custom without searched in package list
    ;; it is not listed as built-in package
    :ensure nil)
  ;; semantic ia is used
  (use-package semantic/ia
    ;; make sure it is loaded and custom without searched in package list
    ;; it is not listed as built-in package
    :ensure nil

    :custom
    (semanticdb-default-save-directory (concat (file-name-as-directory tqnr-dotemacs-path) "semanticdb"))
    (global-semanticdb-minor-mode t))
  ) ;; (when tqnr-section-mode-cedet-semantic

;; [COMMENT.]
;; [[VARCOMMENT.CEDET ECB (Emacs Code Browser): transform Emacs interface to IDE
;; can display other windows or speedbar to view folder tree, source list,
;; variable/function list, buffer history, etc
;; ]]
;; [VARIABLE.tqnr-section-mode-cedet-ecb nil]
(when tqnr-section-mode-cedet-ecb

    ;;
    ;; ECB
  (use-package ecb
    :hook
    ;; hides the extra compile-window directly after the start of ECB
    ;; function from EmacsWiki was not working so I modify it
    (ecb-activate-hook . (lambda ()
       (when (equal 'visible (ecb-compile-window-state))
         (ecb-toggle-compile-window -1))))
    ;; unset hide before show when the ecb window is hidden and after a redraw
    (ecb-hide-ecb-windows-after-hook . (lambda () (setq ecb-hidden-before nil)))
    (ecb-redraw-layout-after-hook . (lambda () (setq ecb-hidden-before nil)))
    ;; let ecb take control of opening of compile window
    (ecb-activate-hook .
      (lambda ()
        (let ((compwin-buffer (ecb-get-compile-window-buffer)))
          (if (not (and compwin-buffer
                     (ecb-compilation-buffer-p compwin-buffer)))
            (ecb-toggle-compile-window -1)))))
    (compilation-filter-hook .
        (lambda ()
          (ecb-goto-window-compilation)
          (end-of-buffer)))

    :config
    ;; to prevent ecb failing to start up
    (setq ecb-version-check nil)

    ;; after create directories buffer
    (add-hook 'ecb-directories-buffer-after-create-hook
      '(lambda ()
         ;; remap enter only in ecb directories buffer
         (local-set-key         (kbd "RET")             'ecb-directories-select)
         (local-set-key         (kbd "M-RET")           'ecb-dired-directory)
         (local-set-key         (kbd "ESC")             'ecb-cancel)
         (local-set-key         (kbd "M-q")             'ecb-toggle-maximize)
         (local-set-key         (kbd "M-r")             'ecb-update-directories-buffer)
         ;; hide/show ecb compile window
         (local-set-key         (kbd "<f2>")            'ecb-toggle-compile)
         ))

    ;; after create sources buffer
    (add-hook 'ecb-sources-buffer-after-create-hook
      '(lambda ()
         ;; remap enter only in ecb sources buffer
         (local-set-key         (kbd "RET")             'ecb-select)
         (local-set-key         (kbd "M-RET")           'ecb-sources-select)
         (local-set-key         (kbd "ESC")             'ecb-cancel)
         (local-set-key         (kbd "M-a")             'ecb-toggle-maximize)
         ))

    ;; after create methods buffer
    (add-hook 'ecb-methods-buffer-after-create-hook
      '(lambda ()
         ;; remap enter only in ecb methods buffer
         (local-set-key         (kbd "RET")             'ecb-select)
         (local-set-key         (kbd "ESC")             'ecb-cancel)
         (local-set-key         (kbd "M-\\")            'ecb-toggle-maximize)
         ))

    ;; after create history buffer
    (add-hook 'ecb-history-buffer-after-create-hook
      '(lambda ()
         ;; remap enter only in ecb history buffer
         (local-set-key         (kbd "RET")             'ecb-select)
         (local-set-key         (kbd "ESC")             'ecb-cancel)
         (local-set-key         (kbd "M-z")             'ecb-toggle-maximize)
         ))

    :custom
    (ecb-auto-activate t)

    ;; set version (only for check install)
    (ecb-options-version "2.40")

    ;; [VARCOMMENT.set default path in "ecb directories"]
    ;; [[VARIABLE.tqnr-profile-ecb-source-path
    ;;   '(
    ;;      ;; before is put the ede projects (see project.el)
    ;;      ("c:/path/i/want/to/have/in/ecb/directory/"        "display name")
    ;;      ("c:/second/path/i/want/to/have/in/ecb/directory/" "display name2")
    ;;      )
    ;; ]]
    (ecb-source-path tqnr-profile-ecb-source-path)

    ;; [VARCOMMENT.regexp of folder to exclude in "ecb directories"]
    ;; [[VARIABLE.tqnr-profile-ecb-excluded-directories-regexps
    ;;   '(
    ;;      "^\\.+$"             ; hidden folder
    ;;      "\\(TOTO\\|TITI\\)$" ; example
    ;;      )
    ;; ]]
    (ecb-excluded-directories-regexps tqnr-profile-ecb-excluded-directories-regexps)

    ;; [VARCOMMENT.files to be ignored in "ecb source" !! RTFM !!]
    ;; [[VARIABLE.tqnr-profile-ecb-source-file-regexps
    ;;   '((".*"
    ;;       ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\)$\\)\\)")
    ;;       ("^\\.\\(emacs\\|gnus\\)$")
    ;;       ))
    ;; ]]
    (ecb-source-file-regexps tqnr-profile-ecb-source-file-regexps)

    ;; [VARCOMMENT.files to be ignored from Version Control VC]
    ;; [[VARIABLE.tqnr-profile-ecb-sources-exclude-cvsignore
    ;;   '(
    ;;      "example"
    ;;      )
    ;; ]]
    (ecb-sources-exclude-cvsignore tqnr-profile-ecb-sources-exclude-cvsignore)

    ;; [VARCOMMENT.regexp to form group in "ecb history"]
    ;; [[VARIABLE.tqnr-profile-ecb-history-make-buckets
    ;;   '(
    ;;      "include" ; all file from an include folder
    ;;      "\\.[hc][p]*$" ; all c cpp and h files
    ;;      "\\.el$" ; all elisp files
    ;;      )
    ;; ]]
    (ecb-history-make-buckets tqnr-profile-ecb-history-make-buckets)

    ;; ask each time I kill a buffer if I want keep it in "ecb history"
    ;; a pain to ask each time...
    ;;(ecb-kill-buffer-clears-history (quote ask))

    ;; how to show functions in "ecb methods"
    (ecb-show-tags
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
    (ecb-type-tag-expansion
      (quote ((default
                "class"
                "interface"
                "group"
                "namespace")
               (c-mode))
        ))

    ;; disable Version Control for ecb
    (ecb-vc-enable-support nil)

    ;; show current function inf modeline
    ;; see semantic
    ;;(which-function-mode t)

    ;; add all buffers name I want in the compile window of ecb
    (ecb-compilation-buffer-names
      (quote (
               ;;FIXME working environment default
               ("*Calculator*")
               ("*vc*")
               ("*vc-diff*")
               ("*Apropos*")
               ("*Occur*")
               ("*shell*")
               ("*Completions*")
               ("\\s-*\\*[cC]ompletion.*\\*\\s-*" . t)
               ("\\*[cC]ompilation.*\\*" . t)
               ("\\*i?grep.*\\*" . t)
               ("*JDEE Compile Server*")
               ("*Help*")
               ("*Backtrace*")
               ("*Compile-log*")
               ("*bsh*")
               ("*Messages*")
               ("msg.txt.*" . t)
               ("*ccm*")
               ("\\*GTAGS SELECT\\*.*" . t)
               ("*Bookmark List*")
               ("*Shell Command Output*")
               ("*Semantic Context Analyze*")
               ("*Macroexpansion*")
               ("\\*Symref .*" . t)
               ("*ECB Analyse*")
               ("\\*[Aa]ck.*\\*" . t)
               ("*Kill Ring*")
               ("\\s-*\\*undo-tree\\*" . t)
               ("\\*.*helm.*\\*" . t)
               ("*clearcase-config-spec*")
               ("*ELP Profiling Results*")
               ("\\s-*\\*[mM]agit.*\\*\\s-*" . t)
               ("*[Ss]ynergy*" . t)
               )))

    ;; do not ignore special display
    (ecb-ignore-special-display nil)

    ;; auto expand tree
    (ecb-auto-expand-tag-tree-collapse-other (quote only-if-on-tag))
    (ecb-expand-methods-switch-off-auto-expand nil)

    ;; do not prescan the directory for emptiness
    (ecb-prescan-directories-for-emptyness nil)

    ;; do not process file without semantic
    (ecb-process-non-semantic-files nil)

    ;; tags file can be modified when a file is opened
    (tags-loop-revert-buffers nil)
    ;;
    ;; color of lines find in *Tag List*
    (tags-tag-face (quote match))

    (ecb-other-window-behavior (quote edit-and-compile))

    :bind
    ;; hide/show ecb window
    ("M-1" . ecb-toggle-ecb-windows)

    ;; hide/show ecb compile window
    ("<f2>" . ecb-toggle-compile)
    ("<mouse-5>" . ecb-toggle-compile-window)

    ;; increase/decrease width of ecb window
    ("C-c w" . ecb-toggle-width)

    ;; got to the ecb directory window
    ("M-q" . ecb-open-directories)
    ;; got to the ecb source window
    ("M-a" . ecb-open-sources)
    ;; got to the ecb method window (function/variable...)
    ("M-\\" . ecb-open-methods)
    ;; got to the ecb history window (all opened file, not all buffers)
    ("M-z" . ecb-open-history)

    ;; go back thanks to ecb
    ("<M-kp-multiply>" . ecb-nav-goto-previous)
    ) ;; (use-package ecb

  ) ;; (when tqnr-section-mode-cedet-ecb


(provide '02-mode-006-cedet)

;;; 02-mode-006-cedet.el ends here
