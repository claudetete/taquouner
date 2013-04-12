;;; mode-ecb.el --- a config file for ecb mode settings

;; Copyright (c) 2010-2013 Claude Tete
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
;; Version: 2.8
;; Created: August 2010
;; Last-Updated: April 2013

;;; Commentary:
;;
;; load by `mode.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-mode-cedet-ecb'
;;              var     `section-mode-cedet'
;;
;; INSTALL
;; do not forget to run cedet before and run ecb-byte-compile to finish

;;; Change Log:
;; 2013-04-12 (2.8)
;;    add browse kill ring + undo tree + helm mode + config spec mode + elp mode
;;    in compile window
;; 2012-12-27 (2.7)
;;    update dot emacs path
;; 2012-12-03 (2.6)
;;    add ack buffer to the compile window
;; 2012-07-09 (2.5)
;;    robustness
;; 2012-06-26 (2.4)
;;    try new small branch of ecb
;; 2012-06-21 (2.3)
;;    patch section to hide ecb compile window + unset variable used by custom
;;    function hook to goto-ecb-window-...
;; 2012-06-12 (2.2)
;;    try a patched ecb snapshot (by Alex Ott) with Emacs 24
;; 2012-06-08 (2.1)
;;    clean up
;; 2012-06-05 (2.0)
;;    remove all profile dependances + remove dead source
;; 2012-05-29 (1.9)
;;    add new filter for source file for AT profile
;; 2012-05-24 (1.8)
;;    do not put all *buffer* in compile window, that fix bug with ispell
;; 2012-05-04 (1.7)
;;    add new path for directories
;; 2012-04-20 (1.6)
;;    add working environment default
;; 2012-03-29 (1.5)
;;    translate comments in English + add some fix
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
(if (and section-environment-os-recognition running-on-emacs-24)
  ;; to avoid error with Emacs 24.1
  ;; or replace in ecb.el:1219
  ;;   "(let ((stack-trace-on-error stack-trace-on-error))"
  ;; by
  ;;   "(let ()"
  (setq stack-trace-on-error t))

(if nil
  (add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "plugins/ecb-alexott"))
  (add-to-list 'load-path (concat (file-name-as-directory dotemacs-path) "plugins/ecb-snap"))
  )

;; hides the extra compile-window directly after the start of ECB
;; function from EmacsWiki was not working so I modify it
(add-hook 'ecb-activate-hook
  (lambda ()
    (when (equal 'visible (ecb-compile-window-state))
      (ecb-toggle-compile-window -1))))

;;
;;; ECB
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

    ;; set default path in "ecb directories"
    '(ecb-source-path profile-ecb-source-path)

    ;; regexp of folder to exclude in "ecb directories"
    '(ecb-excluded-directories-regexps profile-ecb-excluded-directories-regexps)

    ;; files to be ignored in "ecb source" !! RTFM !!
    '(ecb-source-file-regexps profile-ecb-source-file-regexps)

    ;; files to be ignored from Version Control VC
    '(ecb-sources-exclude-cvsignore profile-ecb-sources-exclude-cvsignore)

    ;; regexp to form group in "ecb history"
    '(ecb-history-make-buckets profile-ecb-history-make-buckets)

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
                )))

    ;; auto expand tree
    '(ecb-auto-expand-tag-tree-collapse-other (quote only-if-on-tag))
    '(ecb-expand-methods-switch-off-auto-expand nil)

    ;; do not prescan the directory for emptiness
    '(ecb-prescan-directories-for-emptyness nil)

    ;; do not process file without semantic
    '(ecb-process-non-semantic-files nil)

    ;; tags file can be modified when a file is opened
    '(tags-loop-revert-buffers t)
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
   )


(provide 'mode-ecb)

;;; mode-ecb.el ends here
