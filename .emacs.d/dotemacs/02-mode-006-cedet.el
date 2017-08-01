;;; 02-mode-006-cedet.el --- configuration of cedet mode

;; Copyright (c) 2017 Claude Tete
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
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER."Collection of Emacs Development Environment Tools"]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    merge from split of old mode.el
;; 2016-09-28 (1.8)
;;    enable semantic/ia + patch list of major mode to remove python
;;    (manage by elpy)
;; 2012-12-27 (1.7)
;;    update dot emacs path
;; 2012-06-12 (1.6)
;;    clean up
;; 2012-06-05 (1.5)
;;    use require to load project
;; 2012-03-30 (1.4)
;;    translate comments in English
;; 2012-03-28 (1.3)
;;    disable idle completion and local symbol highlight
;; 2012-03-20 (1.2)
;;    add settings + use gnu global to locate file
;; 2012-03-02 (1.1)
;;    split in two settings for emacs included cedet and cedet from repository
;;    (bzr)
;; 2011-08-10 (1.0)
;;    add gnu global to semantic


;;; Code:

;; to remove warning:
;; Warning: cedet-called-interactively-p called with 0 arguments, but requires 1
(setq byte-compile-warnings nil)

;; [[VARCOMMENT.if you want to use emacs included CEDET set to nil
;; otherwise set the path of cedet.el and you need to remove:
;;   `your-emacs-path/lisp/cedet'
;;   `your-emacs-path/lisp/speedbar.*'
;;   `your-emacs-path/lisp/emacs-lisp/eieio*'
;; ]]
;; [VARIABLE.tqnr-profile-cedet-path nil]
(if tqnr-profile-cedet-path
  (progn
    (message "* CEDET bzr")
    (defvar tqnr-is-cedet-bzr t)
    )
  (progn
    (message "* CEDET built-in")
    (defvar tqnr-is-cedet-bzr nil)
    )
  )

;; init the state of the loading of cedet
(defvar tqnr-is-cedet-loaded nil)
(if tqnr-is-cedet-bzr
  ;; load from the path clt-cedet-path
  (progn
    (if (load-file tqnr-profile-cedet-path)
      (setq tqnr-is-cedet-loaded t)
      (message "    cedet was not loaded. Have you removed your-emacs-path/lisp/cedet/ your-emacs-path/lisp/speedbar.* and your-emacs-path/lisp/emacs-lisp/eieio* ?")
      )
    )
  ;; load from Emacs built-in cedet
  (progn
    (if (try-require 'cedet "    ")
      (setq tqnr-is-cedet-loaded t)
      (message "    cedet was not loaded. Have you GNU/Emacs 23.4 or 24.x ?")
      )
    )
  )

;; only if cedet can be loaded
(when tqnr-is-cedet-loaded
  ;; [VARCOMMENT.bin path of gnu global for cedet]
  ;; [VARIABLE.tqnr-profile-gnu-global (concat tqnr-dotemacs-path "/plugins/gnu_global_628wb/bin/global.exe")]
  ;; [VARIABLE.tqnr-profile-gnu-global-gtags (concat tqnr-dotemacs-path "/plugins/gnu_global_628wb/bin/gtags.exe")]
  (setq cedet-global-command tqnr-profile-gnu-global)
  (setq cedet-global-gtags-command tqnr-profile-gnu-global-gtags)

  ;; [COMMENT.]
  ;; [VARCOMMENT.CEDET SEMANTIC: can do tag, list of function/variable..., preproc, etc]
  ;; [VARIABLE.tqnr-section-mode-cedet-semantic t]
  (when tqnr-section-mode-cedet-semantic
    ;;
    ;; CEDET bzr (snapshot)
    (if tqnr-is-cedet-bzr
      (progn
        ;; additional features for names completion, and displaying of information
        ;; for tags & classes
        ;; package.
        (require 'semantic-ia)

        (custom-set-variables
          ;; set option to locate a file with GNU Global
          '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))

          ;; disable highlight local symbol mode
          '(global-semantic-idle-local-symbol-highlight-mode nil nil (semantic-idle))

          ;; disable completions, I don't use it
          '(global-semantic-idle-completions-mode nil nil (semantic-idle))
          )

        ;; launch ede to have the file project management
        (global-ede-mode t)

        ;; Enabling Semantic (code-parsing, smart completion) features
        ;; Select one of the following:

        ;;;; * This enables the database and idle reparse engines
        ;;(semantic-load-enable-minimum-features)

        ;;;; * This enables some tools useful for coding, such as summary mode
        ;;;;   imenu support, and the semantic navigator
        ;;(semantic-load-enable-code-helpers)

        ;;;; * This enables even more coding tools such as intellisense mode
        ;;;;   decoration mode, and stickyfunc mode (plus regular code helpers)
        ;;(semantic-load-enable-gaudy-code-helpers)

        (semantic-load-enable-excessive-code-helpers)

        ) ; (progn

      ;;
      ;;; CEDET included in emacs (built-in CEDET)
      ;; works under Emacs 24.2 (start without error/warning with ecb)
      (progn
        ;; launch semantic mode
        (semantic-mode t)

        ;; run ede mode to manage project files
        (global-ede-mode t)

        ;;;; set GNU/Global as first database
        ;;(setq ede-locate-setup-options '(ede-locate-global ede-locate-base)) ; do not work ?


        ;; [VARCOMMENT.list of projects: the order is important, display in reverse order (first->last)]
        ;; [VARIABLE.tqnr-profile-ede-project '()]
        (while tqnr-profile-ede-project
          (let (file)
            (setq file (pop tqnr-profile-ede-project))
            (if (file-exists-p file)
              (load-file file)
              (display-warning 'tqnr-profile-ede-project (format "file not found: %s" file) :warning)
              )
            )
          )


        ;; semanticdb is used
        (try-require 'semantic/db "      ")
        ;; semantic ia is used
        (try-require 'semantic/ia "      ")

        ;;;; load GNU/Global for semantic
        ;;(try-require 'semantic/db-global)
        ;;;; semantic database is used in c and c++ mode
        ;;(semanticdb-enable-gnu-global-databases 'c-mode)
        ;;(semanticdb-enable-gnu-global-databases 'c++-mode)

        (custom-set-variables

          ;; SEMANTIC SETTING
          ;; choose the semanticdb at submode
          '(semantic-default-submodes
             '(
                global-semantic-highlight-func-mode
                global-semantic-decoration-mode
                global-semantic-stickyfunc-mode
                global-semantic-idle-scheduler-mode
                global-semanticdb-minor-mode))

          ;; do not use image with semantic
          '(semantic-format-use-images-flag nil)

          ;; set the path of the database
          '(semanticdb-default-save-directory (concat (file-name-as-directory tqnr-dotemacs-path) "semanticdb"))

          '(global-semanticdb-minor-mode t)

          ;; remove python mode in this list (to work with elpy mode)
          '(semantic-new-buffer-setup-functions
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

          ;; show the function name in the header line
          ;'(global-semantic-stickyfunc-mode t)
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
        ) ; (progn
      ) ; (if tqnr-is-cedet-bzr


    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        ;; go to the tag with shift + left click
        (global-set-key         (kbd "<S-down-mouse-1>")        'ignore)
        (global-set-key         (kbd "<S-mouse-1>")             (lambda (event)
                                                                  ;; the function get a event parameter (from mouse click)
                                                                  (interactive "e")
                                                                  ;; set the point at the mouse position
                                                                  (mouse-set-point event)
                                                                  (semantic-goto-definition (point))))

        (when (not tqnr-section-mode-helm)
          ;; go to the tag
          (global-set-key       (kbd "M-.")                     'semantic-goto-definition)

          ;; find all references of a symbol by regexp
          (global-set-key       (kbd "C-M-.")                   'semantic-symref-regexp)

          ;; return back after "go to the tag"
          ;; you can use the default shortcut: M-xB, which ask where you want go back
          (global-set-key       (kbd "C-<")                     'semantic-pop-tag-mark)
          (global-set-key       (kbd "<mouse-4>")               'semantic-pop-tag-mark)
          ) ; (when (not tqnr-section-mode-helm)

        ;; right click will open menu with list of variable/function/include
        (global-set-key         (kbd "<mouse-3>")               'imenu)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook

    ) ;; (when tqnr-section-mode-cedet-semantic

  ;; [COMMENT.]
  ;; [[VARCOMMENT.CEDET ECB (Emacs Code Browser): transform Emacs interface to IDE
  ;; can display other windows or speedbar to view folder tree, source list,
  ;; variable/function list, buffer history, etc
  ;; ]]
  ;; [VARIABLE.tqnr-section-mode-cedet-ecb nil]
  (when tqnr-section-mode-cedet-ecb
    ;; path of ecb
    (when (and tqnr-section-environment-os-recognition tqnr-running-on-emacs-24)
      ;;;; to avoid error with Emacs 24.x
      ;;;; or replace in ecb.el:1219
      ;;;;   "(let ((stack-trace-on-error stack-trace-on-error))"
      ;;;; by
      ;;;;   "(let ()"
      ;;(setq stack-trace-on-error t)
      )

    (if nil
      (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/ecb-alexott"))
      (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/ecb-snap"))
      )

    ;; hides the extra compile-window directly after the start of ECB
    ;; function from EmacsWiki was not working so I modify it
    (add-hook 'ecb-activate-hook
      (lambda ()
        (when (equal 'visible (ecb-compile-window-state))
          (ecb-toggle-compile-window -1))))

    ;;
    ;; ECB
    (when (try-require 'ecb "      ")
      ;; to prevent ecb failing to start up
      (setq ecb-version-check nil)

      ;; unset hide before show when the ecb window is hidden and after a redraw
      (add-hook 'ecb-hide-ecb-windows-after-hook '(lambda () (setq ecb-hidden-before nil)))
      (add-hook 'ecb-redraw-layout-after-hook '(lambda () (setq ecb-hidden-before nil)))

      (custom-set-variables
        '(ecb-auto-activate t)

        ;; set version (only for check install)
        '(ecb-options-version "2.40")

        ;; [VARCOMMENT.set default path in "ecb directories"]
        ;; [VARIABLE.tqnr-profile-ecb-source-path '()]
        '(ecb-source-path tqnr-profile-ecb-source-path)

        ;; [VARCOMMENT.regexp of folder to exclude in "ecb directories"]
        ;; [VARIABLE.tqnr-profile-ecb-excluded-directories-regexps '()]
        '(ecb-excluded-directories-regexps tqnr-profile-ecb-excluded-directories-regexps)

        ;; [VARCOMMENT.files to be ignored in "ecb source" !! RTFM !!]
        ;; [VARIABLE.tqnr-profile-ecb-source-file-regexps '(())]
        '(ecb-source-file-regexps tqnr-profile-ecb-source-file-regexps)

        ;; [VARCOMMENT.files to be ignored from Version Control VC]
        ;; [VARIABLE.tqnr-profile-ecb-sources-exclude-cvsignore '()]
        '(ecb-sources-exclude-cvsignore tqnr-profile-ecb-sources-exclude-cvsignore)

        ;; [VARCOMMENT.regexp to form group in "ecb history"]
        ;; [VARIABLE.tqnr-profile-ecb-history-make-buckets '()]
        '(ecb-history-make-buckets tqnr-profile-ecb-history-make-buckets)

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

        ;; add all buffers name I want in the compile window of ecb
        '(ecb-compilation-buffer-names
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
        '(ecb-ignore-special-display nil)

        ;; auto expand tree
        '(ecb-auto-expand-tag-tree-collapse-other (quote only-if-on-tag))
        '(ecb-expand-methods-switch-off-auto-expand nil)

        ;; do not prescan the directory for emptiness
        '(ecb-prescan-directories-for-emptyness nil)

        ;; do not process file without semantic
        '(ecb-process-non-semantic-files nil)

        ;; tags file can be modified when a file is opened
        '(tags-loop-revert-buffers nil)
        ;;
        ;; color of lines find in *Tag List*
        '(tags-tag-face (quote match))

        '(ecb-other-window-behavior (quote edit-and-compile))
        ) ; (custom-set-variables

      ;; let ecb take control of opening of compile window
      (add-hook 'ecb-activate-hook
        (lambda ()
          (let ((compwin-buffer (ecb-get-compile-window-buffer)))
            (if (not (and compwin-buffer
                       (ecb-compilation-buffer-p compwin-buffer)))
              (ecb-toggle-compile-window -1)))))
      (add-hook 'compilation-filter-hook
        (lambda ()
          (ecb-goto-window-compilation)
          (end-of-buffer)))
      ) ; (when (try-require 'ecb)

    ;; The advised version of switch-to-buffer-other-window can redraw the layout
    ;; (e.g. if the buffer in the compile-window is the slave and the
    ;; compile-window has been made visible), so <window> in the code below can be
    ;; a destroyed window-object! we have to prevent from this (e.g. by selecting
    ;; the window before by number).
      ;;;; I don't know if this do something ??
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
      ) ;; (when-ecb-running-emacs


    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        ;; hide/show ecb window
        (global-set-key         (kbd "M-1")             'ecb-toggle-ecb-windows)

        ;; hide/show ecb compile window
        (global-set-key         (kbd "<f2>")            'ecb-toggle-compile)
        (global-set-key         (kbd "<mouse-5>")       'ecb-toggle-compile-window)

        ;; increase/decrease width of ecb window
        (global-set-key         (kbd "C-c w")           'ecb-toggle-width)

        ;; got to the ecb directory window
        (global-set-key         (kbd "M-q")             'ecb-open-directories)
        ;; got to the ecb source window
        (global-set-key         (kbd "M-a")             'ecb-open-sources)
        ;; got to the ecb method window (function/variable...)
        (global-set-key         (kbd "M-\\")            'ecb-open-methods)
        ;; got to the ecb history window (all opened file, not all buffers)
        (global-set-key         (kbd "M-z")             'ecb-open-history)

        ;; go back thanks to ecb
        (global-set-key         (kbd "<M-kp-multiply>") 'ecb-nav-goto-previous)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook

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

    ) ;; (when tqnr-section-mode-cedet-ecb
  ) ;; (when tqnr-is-cedet-loaded

(provide '02-mode-006-cedet)

;;; 02-mode-006-cedet.el ends here
2
