;;; 02-mode-106-eglot.el --- configuration of eglot -*- lexical-binding: t -*-

;; Copyright (c) 2023 Claude Tete
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
;; Version: 0.1
;; Created: March 2023
;; Last-Updated: March 2023

;;; Commentary:
;;
;; [SUBHEADER.eglot config]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package eglot
  :ensure t
  :preface
  ;; Workaround for https://github.com/AdaCore/ada_language_server/issues/1204
  (defun init.el/fix-eol/eglot--format-markup (value)
    (let ((strings (list value)))
      (dolist (eol '("\r\n" "\n" "\r"))
        (setq strings
              (flatten-list (mapcar (lambda (value)
                                      (split-string value eol))
                                    strings))))
      (string-join strings "\n")))
  ;; Workaround for https://github.com/joaotavora/eglot/discussions/1467
  (defun init.el/multiline/eglot-hover-eldoc-function (r)
    (list
     (lambda (info &rest _ignore)
       ;; Ignore the `eglot-hover-eldoc-function' provided ":echo"
       ;; cookie in order to display multi-line documentation.
       (funcall (car r) info))))

  ;; ;; Workaround completion issues ??
  ;; (defun init.el/around-advice/eglot-completion-at-point (oldfun &rest _)
  ;;   (when (or (null init.el/completion-lsp-disallowed-contexts)
  ;;             (let ((status (syntax-ppss)))
  ;;               (seq-every-p
  ;;                (lambda (context)
  ;;                  (pcase context
  ;;                    ('comment (not (nth 4 status)))
  ;;                    ('string  (not (nth 3 status)))
  ;;                    (_        (error "Unknown context: %s" context))))
  ;;                init.el/completion-lsp-disallowed-contexts)))
  ;;     (pcase (funcall oldfun)
  ;;       (`(,beg ,end ,table . ,plist)
  ;;        (let (settings case-fold-table)
  ;;          ;; Allow next completion at point function (CAPF) specified
  ;;          ;; in `completion-at-point-functions' to be queried when
  ;;          ;; Language Server doesn't have any completions at point.
  ;;          (setq settings (append settings '(:exclusive no)))
  ;;          ;; When configured, prevent LSP trigger characters from
  ;;          ;; initiating completion before hitting the threshold
  ;;          ;; specified by `init.el/completion-minimum-prefix-length'.
  ;;          (unless init.el/completion-lsp-allow-trigger-chars
  ;;            (setq settings (append settings '(:company-prefix-length nil))))
  ;;          ;; Perform case folding when matching against completion results
  ;;          (setq case-fold-table (completion-table-case-fold table))
  ;;          `(,beg ,end ,case-fold-table ,@settings ,@plist))))))

  :init
  (advice-add 'eglot--format-markup
              :filter-return #'init.el/fix-eol/eglot--format-markup)
  (advice-add 'eglot-hover-eldoc-function
              :filter-args #'init.el/multiline/eglot-hover-eldoc-function)
  ;; (advice-add 'eglot-completion-at-point
  ;;             :around #'init.el/around-advice/eglot-completion-at-point)

  :config
  ;; [VARCOMMENT.Use eglot to manage ada-mode]
  ;; [VARIABLE.tqnr-section-mode-eglot-ada nil]
  (when tqnr-section-mode-eglot-ada
    (add-hook 'ada-mode-hook 'eglot-ensure))
  ;; [VARCOMMENT.Use eglot to manage python-mode]
  ;; [VARIABLE.tqnr-section-mode-eglot-python nil]
  (when tqnr-section-mode-eglot-python
    ;; (add-to-list 'eglot-server-programs
    ;;              '((python-mode python-ts-mode)
    ;;                "basedpyright-langserver" "--stdio"))
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode)
                   "ty" "server"))
    (add-hook 'python-mode-hook 'eglot-ensure))
  ;; Let major mode control Imenu
  (add-to-list 'eglot-stay-out-of 'imenu)
  ;; Prefer "basic" completion style instead of the default
  ;; "eglot--dumb-flex".
  (add-to-list 'completion-category-overrides
               '(eglot-capf (styles . (basic))))
  ;; async connection to not block the Emacs UI
  (setq eglot-sync-connect nil)

  ;; Disable event logging completely
  (when (>= emacs-major-version 30)
    ;; (Emacs >= 30)
    (setq eglot-events-buffer-config '(:size 0 :format short)))

  (when (<= emacs-major-version 29)
    ;; For Emacs <= 29
    (setq eglot-events-buffer-size 0))

  ;; shuts down the LSP server after the last managed killed buffer
  (setq eglot-autoshutdown t)

  ;; Disable automatic code action indicators to reduce background polling
  (setq eglot-code-action-indications nil)

  ;; increase the maximum amount of data Emacs reads from a subprocess in a single operation
  (setq read-process-output-max (* 1024 1024))

  ;; disable Eglot's on-type formatting
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)

  :custom
  ;; do not display mulitline in echo area, use
  (eldoc-echo-area-use-multiline-p nil)

  :bind
  (:map eglot-mode-map
        ;; display doc at point
	("C-?" . eldoc)
        ;; pretty print the file
	("C-+" . eglot-format)
        ;; back button on mouse to go back after browsing via xref
        ("<mouse-8>" . pop-tag-mark)
        ;; find all ref at point
        ("C-M-." . xref-find-references)
        )
  ) ;; (use-package eglot

(use-package eglot-hierarchy
  ;; make sure it is loaded and custom without searched in package list
  :ensure nil
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/"))

  :bind
  ("M->" . eglot-hierarchy-call-hierarchy)

  :custom
  (eglot-hierarchy-call-site t)
  )

;; (use-package eglot-booster
;;   :after eglot
;;   :config (eglot-booster-mode))

(use-package xref
  :ensure nil ; built-in
  :preface
  (defun init.el/fix-point/xref-find-definitions-at-mouse (event)
    (interactive "e")
    (mouse-set-point event))
  :init
  ;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=65578
  (when (< emacs-major-version 30)
    (advice-add 'xref-find-definitions-at-mouse
                :before #'init.el/fix-point/xref-find-definitions-at-mouse))
  :bind (("C-<mouse-1>" . #'ignore) ; Prevent "undefined" message on mouse up
         ("C-<down-mouse-1>" . #'xref-find-definitions-at-mouse)))


(provide '02-mode-106-eglot)

;;; 02-mode-106-eglot.el ends here
