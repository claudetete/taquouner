;;; package --- summary  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;;;; Configuration

(defgroup init.el nil
  "Configuration options for init.el."
  :group 'emacs
  :prefix "init.el/")

(defcustom init.el/completion-lsp-allow-trigger-chars t
  "Allow LSP completion trigger characters to display completion UI.

This will allow Language Server specified trigger characters to
automatically display the completion UI, even if
`init.el/completion-minimum-prefix-length' has not been met."
  :type 'boolean
  :group 'init.el)

(defcustom init.el/completion-lsp-disallowed-contexts '(comment string)
  "Disallow LSP completion within specified contexts."
  :type '(set (const :tag "Avoid completion in comments" comment)
              (const :tag "Avoid completion in strings"  string))
  :group 'init.el)

(defcustom init.el/completion-quick-access nil
  "Display quick access index in completion UI.

When quick access is enabled, the corresponding completion item can be
selected via M-<i>, where <i> corresponds to the index displayed next to
the completion item."
  :type 'boolean
  :group 'init.el)

(defcustom init.el/completion-minimum-prefix-length 2
  "Minimum prefix length before displaying the completion UI.

Completion can always be started manually, but in order to automatically
display the completion UI, this prefix length should be met."
  :type 'integer
  :group 'init.el)

(defcustom init.el/preferred-completion-ui 'corfu
  "Preferred completion User Interface."
  :type '(choice (const corfu)
                 (const company))
  :group 'init.el)

(defcustom init.el/preferred-diagnostics-reporter 'flymake
  "Preferred diagnostics reporter."
  :type '(choice (const flymake)
                 (const flycheck))
  :group 'init.el)

(defcustom init.el/preferred-lsp-client 'lsp-mode
  "Preferred LSP client."
  :type '(choice (const lsp-mode)
                 (const eglot))
  :group 'init.el)

(defcustom init.el/preferred-documentation-ui 'box
  "Preferred LSP documentation UI."
  :type '(choice (const :tag "Child Frame" box)
                 (const :tag "Echo Area"   echo))
  :group 'init.el)

;;;; Configuration

;; Key bindings to easily locate user configuration.
(use-package emacs
  :ensure nil ; built-in
  :preface
  (defun init.el/find-file-user-init ()
    (interactive)
    (find-file (locate-user-emacs-file "init.el")))
  (defun init.el/find-file-user-early-init ()
    (interactive)
    (find-file (locate-user-emacs-file "early-init.el")))
  :bind (("<f12>"   . init.el/find-file-user-init)
         ("S-<f12>" . init.el/find-file-user-early-init)))

;;;; Custom

;; Don't pollute this file with custom settings
(setopt custom-file (locate-user-emacs-file "custom-settings.el"))
(load custom-file 'noerror)

;;;; use-package

(setopt use-package-always-ensure t)
(setopt use-package-always-defer t)
(setopt use-package-enable-imenu-support t)

;;;; package.el

(use-package package
  :ensure nil ; built-in
  :config
  (add-to-list 'package-archives '("melpa"     . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/")))

;;;; Ada

(use-package ada-ts-mode
  :defines (ada-ts-mode-map consult-imenu-config org-src-lang-modes)
  :custom ((ada-ts-mode-grammar-install 'auto)
           (ada-ts-mode-indent-backend 'lsp)) ; Use LSP-based indenting
  :bind (:map ada-ts-mode-map
              (("C-c C-b" . ada-ts-mode-defun-comment-box)
               ("C-c C-o" . ada-ts-mode-find-other-file)
               ("C-c C-p" . ada-ts-mode-find-project-file)))
  :init
  (with-eval-after-load 'consult-imenu
    (add-to-list
     'consult-imenu-config
     '(ada-ts-mode :types ((?p "Package")
                           (?s "Subprogram" font-lock-function-name-face)
                           (?t "Type Declaration" font-lock-type-face)
                           (?w "With Clause")))))
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("ada" . ada-ts))))

;;;; GNAT Project

(use-package gpr-ts-mode
  :defines (consult-imenu-config org-src-lang-modes)
  :custom (gpr-ts-mode-grammar-install 'auto)
  :init
  (with-eval-after-load 'consult-imenu
    (add-to-list
     'consult-imenu-config
     '(gpr-ts-mode :types ((?a "Attribute")
                           (?P "Project")
                           (?p "Package")
                           (?t "Type")
                           (?v "Variable")
                           (?w "With Clause")))))
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("gpr" . gpr-ts)))
  :hook (gpr-ts-mode . gpr-ts-auto-case-mode))

;;;; Completion

(use-package emacs
  :ensure nil ; built-in
  :custom (tab-always-indent 'complete)) ; Complete when already indented

(setq completion-ignore-case t)

;;;;; Completion at Point

;;;;;; Company

(use-package company
  :if (eq init.el/preferred-completion-ui 'company)
  :demand t
  :commands (global-company-mode)
  :defines (company-mode-map company-active-map)
  :functions (company-indent-or-complete-common)
  :preface
  ;; Disable Company's de-duplication functionality since overloaded
  ;; functions might be collapsed into a single entry.  This can
  ;; happen, especially for LSP-supplied completions where some of the
  ;; completion information might be lazily gathered, depending on the
  ;; LSP client (e.g., `lsp-mode' and CompletionItem.detail), causing
  ;; there to not be enough initial information for Company's
  ;; de-duplication implementation to see that the server-supplied
  ;; overloads are not duplicates and thus erroneously remove all but
  ;; a single instance.
  (defun init.el/company-capf (oldfun &rest r)
    (unless (eq (car r) 'duplicates)
      (apply oldfun r)))
  (advice-add 'company-capf :around #'init.el/company-capf)
  :bind
  ;; Allow Company to be triggered manually through the normal
  ;; `indent-for-tab-command' binding.  See the following link for
  ;; details: https://emacs.stackexchange.com/a/46792
  (:map company-mode-map
        (([remap indent-for-tab-command] . company-indent-or-complete-common)))
  ;; Use TAB instead of RET to complete
  (:map company-active-map
        (("RET"      . nil) ; remove from map
         ("<return>" . nil) ; remove from map
         ("TAB"      . company-complete-selection)
         ("<tab>"    . company-complete-selection)))
  :custom ((company-minimum-prefix-length init.el/completion-minimum-prefix-length)
           (company-icon-margin 3)
           (company-require-match nil)
           (company-show-quick-access
            (and init.el/completion-quick-access
                 'left))
           (company-tooltip-align-annotations t))
  :config (global-company-mode))

(use-package company-quickhelp
  :custom ((company-quickhelp-delay 0.0)
           ;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=74807
           (company-quickhelp-use-propertized-text nil))
  :commands (company-quickhelp-local-mode)
  :hook (company-mode . company-quickhelp-local-mode))

;;;;;; Corfu

(use-package corfu
  :if (eq init.el/preferred-completion-ui 'corfu)
  :demand t
  :defines (corfu-map)
  :commands (global-corfu-mode)
  :custom ((corfu-auto t)
           (corfu-auto-delay 0.0)
           (corfu-auto-prefix init.el/completion-minimum-prefix-length)
           (corfu-cycle t))
  :bind (:map corfu-map
              (("RET" . nil))) ; remove from map
  :config (global-corfu-mode))

(use-package corfu-popupinfo
  :ensure corfu ; part of corfu
  :custom (corfu-popupinfo-delay 0.0)
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-indexed
  :if (and (eq init.el/preferred-completion-ui 'corfu)
           init.el/completion-quick-access)
  :ensure corfu ; part of corfu
  :demand t
  :commands (corfu-indexed-mode)
  :defines (corfu--index
            corfu--scroll
            corfu--total
            corfu-count
            corfu-indexed-mode
            corfu-indexed-start)
  :functions (corfu-insert)
  :custom (corfu-indexed-start 1)
  :config
  (dolist (idx (number-sequence 0 (1- corfu-count)))
    (let* ((key (mod (+ idx corfu-indexed-start) 10))
           ;; The bound command is named with a "corfu-" prefix so it
           ;; is matched in `corfu-continue-commands', otherwise Corfu
           ;; will insert the current candidate and exit completion
           ;; (in the `pre-command-hook' -- see `corfu--prepare')
           ;; before the command is executed.
           (name (intern (format "corfu-indexed--M%s" key))))
      (fset name
            (lambda ()
              (interactive)
              (let ((index (+ corfu--scroll idx)))
                (when (and corfu-indexed-mode
                           (< index corfu--total)
                           (< index (+ corfu--scroll corfu-count)))
                  (setq corfu--index index)
                  (corfu-insert)))))
      (define-key corfu-map (kbd (format "M-%s" key)) name)))
  (corfu-indexed-mode))

(use-package svg-lib
  :defines (svg-lib-icon-collections)
  :config
  (add-to-list 'svg-lib-icon-collections
               '("vscode-codicons" . "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg")))

(use-package kind-icon
  :commands (kind-icon-margin-formatter)
  :defines (corfu-margin-formatters)
  :custom
  (kind-icon-mapping
   '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode-codicons")
     (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode-codicons")
     (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode-codicons")
     (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode-codicons")
     (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode-codicons")
     (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode-codicons")
     (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode-codicons")
     (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode-codicons")
     (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode-codicons")
     (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode-codicons")
     (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode-codicons")
     (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode-codicons")
     (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode-codicons")
     (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode-codicons")
     (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode-codicons")
     (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode-codicons")
     (module         "{"   :icon "symbol-namespace"   :face font-lock-type-face              :collection "vscode-codicons")
     (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode-codicons")
     (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode-codicons")
     (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode-codicons")
     (reference      "rf"  :icon "references"         :face font-lock-doc-face               :collection "vscode-codicons")
     (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode-codicons")
     (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode-codicons")
     (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode-codicons")
     (text           "tx"  :icon "symbol-key"         :face shadow                           :collection "vscode-codicons")
     (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode-codicons")
     (unit           "u"   :icon "symbol-ruler"       :face shadow                           :collection "vscode-codicons")
     (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode-codicons")
     (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode-codicons")
     (t              "."   :icon "symbol-property"    :face shadow                           :collection "vscode-codicons")))
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;;;;; Minibuffer Completion

(use-package vertico
  :demand t
  :commands (vertico-mode)
  :config (vertico-mode))

;;;; Compile

(use-package compile
  :ensure nil ; built-in
  :custom (compilation-scroll-output t)
  :init
  (put 'compile-command 'safe-local-variable #'stringp))

;;;; Diagnostics

;;;;; Flycheck

(use-package flycheck
  :if (eq init.el/preferred-diagnostics-reporter 'flycheck)
  :functions (flycheck-overlay-errors-at
              flycheck-help-echo-all-error-messages)
  :preface
  (defun init.el/flycheck-errors-at-point (callback &rest _)
    (when-let ((diagnostics (and (bound-and-true-p flycheck-mode)
                                 (flycheck-overlay-errors-at (point)))))
      (funcall callback
               (flycheck-help-echo-all-error-messages
                diagnostics))))
  ;; Disable intrinsic display function, as Eldoc will be used instead.
  :custom (flycheck-display-errors-function nil)
  :init
  ;; Configure Flycheck to display diagnostics via Eldoc
  (add-hook 'eldoc-documentation-functions
            #'init.el/flycheck-errors-at-point)
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-eglot
  :if (eq init.el/preferred-diagnostics-reporter 'flycheck)
  :custom (flycheck-eglot-exclusive nil)
  :hook (eglot-managed-mode . flycheck-eglot-mode))

(use-package consult-flycheck
  :if (eq init.el/preferred-diagnostics-reporter 'flycheck)
  :bind ("M-g M-d" . consult-flycheck))

;;;;; Flymake

(use-package flymake
  :ensure nil ; built-in
  :if (eq init.el/preferred-diagnostics-reporter 'flymake)
  :hook (prog-mode . flymake-mode))

(use-package consult-flymake
  :ensure consult ; part of consult
  :if (eq init.el/preferred-diagnostics-reporter 'flymake)
  :bind ("M-g M-d" . consult-flymake))

;;;; Documentation

(use-package eldoc
  :ensure nil ; built-in
  :custom (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package eldoc-box
  :if (eq init.el/preferred-documentation-ui 'box)
  :preface
  (defun init.el/eldoc-box-max-pixel-width ()
    ;; Cap child frame width based on parent frame width (so child
    ;; isn't wider than parent), but try to respect the custom default
    ;; width, when possible.
    (defvar eldoc-box-offset)
    (let* ((parent-width (frame-outer-width (selected-frame)))
           (left-offset (nth 0 eldoc-box-offset))
           (right-offset (nth 1 eldoc-box-offset))
           (max-offset (max left-offset right-offset))
           (default-max-width
            (eval (car (get 'eldoc-box-max-pixel-width 'standard-value)))))
      (max 0 (min default-max-width (- parent-width max-offset)))))
  :custom ((eldoc-box-clear-with-C-g t)
           (eldoc-box-max-pixel-width #'init.el/eldoc-box-max-pixel-width)
           (eldoc-box-offset '(32 32 16))
           (eldoc-box-only-multi-line nil))
  :hook (prog-mode . eldoc-box-hover-mode))

;;;; Editor

(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ; Support line comment

;; Remember position in previously visited files.
(use-package saveplace
  :ensure nil ; built-in
  :demand t
  :config (save-place-mode))

;;;; Eglot

(use-package eglot
  :ensure nil ; built-in
  :pin gnu-devel
  :if (eq init.el/preferred-lsp-client 'eglot)
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
  ;; Workaround completion issues
  (defun init.el/around-advice/eglot-completion-at-point (oldfun &rest _)
    (when (or (null init.el/completion-lsp-disallowed-contexts)
              (let ((status (syntax-ppss)))
                (seq-every-p
                 (lambda (context)
                   (pcase context
                     ('comment (not (nth 4 status)))
                     ('string  (not (nth 3 status)))
                     (_        (error "Unknown context: %s" context))))
                 init.el/completion-lsp-disallowed-contexts)))
      (pcase (funcall oldfun)
        (`(,beg ,end ,table . ,plist)
         (let (settings case-fold-table)
           ;; Allow next completion at point function (CAPF) specified
           ;; in `completion-at-point-functions' to be queried when
           ;; Language Server doesn't have any completions at point.
           (setq settings (append settings '(:exclusive no)))
           ;; When configured, prevent LSP trigger characters from
           ;; initiating completion before hitting the threshold
           ;; specified by `init.el/completion-minimum-prefix-length'.
           (unless init.el/completion-lsp-allow-trigger-chars
             (setq settings (append settings '(:company-prefix-length nil))))
           ;; Perform case folding when matching against completion results
           (setq case-fold-table (completion-table-case-fold table))
           `(,beg ,end ,case-fold-table ,@settings ,@plist))))))
  :init
  ;; Force upgrade to ELPA version for Emacs 29
  (unless (or (> emacs-major-version 29)
              (member 'eglot package-selected-packages))
    (package-install (cadr (assoc 'eglot package-archive-contents))))
  (advice-add 'eglot--format-markup
              :filter-return #'init.el/fix-eol/eglot--format-markup)
  (advice-add 'eglot-hover-eldoc-function
              :filter-args #'init.el/multiline/eglot-hover-eldoc-function)
  (advice-add 'eglot-completion-at-point
              :around #'init.el/around-advice/eglot-completion-at-point)
  :hook ((ada-ts-mode gpr-ts-mode) . eglot-ensure)
  :custom (eglot-extend-to-xref t) ; Consider external refs, part of project.
  :config
  ;; Let major mode control Imenu
  (add-to-list 'eglot-stay-out-of 'imenu)
  ;; Prefer "basic" completion style instead of the default
  ;; "eglot--dumb-flex".
  (add-to-list 'completion-category-overrides
               '(eglot-capf (styles . (basic))))
  ;; Add `lsp-mode' language server installation location to
  ;; `exec-path' so Eglot can find it.
  (let* ((os-dir (cond ((eq system-type 'gnu/linux)  "linux")
                       ((eq system-type 'windows-nt) "win32")
                       ((eq system-type 'darwin)     "darwin")))
         (arch-dir (cond ((string-prefix-p "x86_64-"  system-configuration) "x64")
                         ((string-prefix-p "aarch64-" system-configuration) "arm64")))
         ;; >= ALS 25.0.20240915
         (archive-path-2 `("integration" "vscode" "ada" ,arch-dir ,os-dir))
         ;; < ALS 25.0.20240915
         (archive-path-1 `(,arch-dir ,os-dir)))
    (dolist (archive-path `(,archive-path-2 ,archive-path-1))
      (add-to-list 'exec-path
                   (expand-file-name
                    (string-join `(".cache/lsp/ada-ls" ,@archive-path) "/")
                    user-emacs-directory)
                   ;; Add to end of path so ALS found earlier on the
                   ;; path is preferred.
                   'append))))

;;;; Emacs

(use-package emacs
  :ensure nil ; built-in
  :init
  ;; Recommended settings when using LSP
  :custom ((gc-cons-threshold 100000000)             ; 100MB
           (read-process-output-max (* 1024 1024)))) ; 1MB

;;;; Files

(use-package files
  :ensure nil ; built-in
  :defer t
  :custom ((make-backup-files nil)
           (auto-save-default nil)
           (create-lockfiles nil)))

;;;; Imenu

(use-package imenu
  :ensure nil ; built-in
  :custom (imenu-auto-rescan t)
  :init
  (add-to-list 'completion-category-overrides
               '(imenu (styles . (substring))))
  :hook ((ada-ts-mode gpr-ts-mode) . imenu-add-menubar-index))

(use-package consult-imenu
  :ensure consult ; part of consult
  :bind ("M-g M-i" . consult-imenu))

;;;; lsp-mode

(use-package lsp-mode
  :if (eq init.el/preferred-lsp-client 'lsp-mode)
  :preface
  ;; Workaround for https://github.com/AdaCore/ada_language_server/issues/1204
  (defun init.el/fix-eol/lsp--render-string (args)
    (let ((strings (list (car args))))
      (dolist (eol '("\r\n" "\n" "\r"))
        (setq strings
              (flatten-list (mapcar (lambda (value)
                                      (split-string value eol))
                                    strings))))
      (cons (string-join strings "\n") (cdr args))))
  (defun init.el/lsp-mode ()
    ;; Delay start until after initialization of local variables as they may
    ;; contain `lsp-mode' configuration variables.
    (declare-function lsp "lsp-mode")
    (add-hook 'hack-local-variables-hook #'lsp t 'local))
  ;; Workaround completion issues
  (defun init.el/around-advice/lsp-completion-at-point (oldfun &rest _)
    (when (or (null init.el/completion-lsp-disallowed-contexts)
              (let ((status (syntax-ppss)))
                (seq-every-p
                 (lambda (context)
                   (pcase context
                     ('comment (not (nth 4 status)))
                     ('string  (not (nth 3 status)))
                     (_        (error "Unknown context: %s" context))))
                 init.el/completion-lsp-disallowed-contexts)))
      (pcase (funcall oldfun)
        (`(,beg ,end ,table . ,plist)
         (let (settings case-fold-table)
           ;; Allow next completion at point function (CAPF) specified
           ;; in `completion-at-point-functions' to be queried when
           ;; Language Server doesn't have any completions at point.
           (setq settings (append settings '(:exclusive no)))
           ;; When configured, prevent LSP trigger characters from
           ;; initiating completion before hitting the threshold
           ;; specified by `init.el/completion-minimum-prefix-length'.
           (unless init.el/completion-lsp-allow-trigger-chars
             (setq settings (append settings '(:company-prefix-length nil))))
           ;; Perform case folding when matching against completion results
           (setq case-fold-table (completion-table-case-fold table))
           `(,beg ,end ,case-fold-table ,@settings ,@plist))))))
  :init
  (advice-add 'lsp--render-string
              :filter-args #'init.el/fix-eol/lsp--render-string)
  (advice-add 'lsp-completion-at-point
              :around #'init.el/around-advice/lsp-completion-at-point)
  :custom ((lsp-auto-guess-root t)
           (lsp-completion-provider :none)
           (lsp-diagnostics-provider
            (intern (concat ":" (symbol-name init.el/preferred-diagnostics-reporter))))
           (lsp-eldoc-render-all t)
           (lsp-enable-indentation nil) ; Let major mode control indentation
           (lsp-enable-on-type-formatting nil) ; Interferes with Emacs indenting
           (lsp-headerline-breadcrumb-enable nil)
           (lsp-keymap-prefix "C-c l")
           (lsp-modeline-diagnostics-enable nil)
           (lsp-semantic-tokens-enable t)
           (lsp-enable-imenu nil)) ; Let major mode control Imenu
  :custom-face
  (lsp-face-semhl-number ((t (:inherit font-lock-number-face))))
  :hook ((ada-ts-mode gpr-ts-mode) . init.el/lsp-mode)
  :config
  ;; Prefer "basic" completion style instead of the default
  ;; "lsp-passthrough".
  (add-to-list 'completion-category-overrides
               '(lsp-capf (styles . (basic)))))

;;;; Markdown

(use-package markdown-mode
  :defines (markdown-code-lang-modes)
  :config
  ;; Some LSP servers use "plaintext" in code fences (i.e., Ada LS), but
  ;; `markdown-mode' (used by Eglot) doesn't provide a mode mapping and ends up
  ;; using markdown, which can be problematic if markdown syntax is found in the
  ;; text.  Therefore, we add a specific mapping for "plaintext" to text mode.
  (add-to-list 'markdown-code-lang-modes '("plaintext" . text-mode)))

;;;; Project

(use-package project
  :ensure nil ; built-in
  :custom (project-vc-extra-root-markers
           '("adainclude" "alire.toml" ".project" ".projectile" ".vscode")))

;;;; User Interface

(use-package display-line-numbers
  :ensure nil ; built-in
  :custom ((display-line-numbers-grow-only t)
           (display-line-numbers-width-start t))
  :hook (prog-mode . display-line-numbers-mode))

(use-package emacs
  :ensure nil ; built-in
  :bind ([remap kill-buffer] . kill-current-buffer) ; Don't prompt
  :custom (use-short-answers t))

(use-package tool-bar
  :ensure nil ; built-in
  :demand t
  :config (tool-bar-mode -1))

(use-package which-func
  :ensure nil ; built-in
  :demand t
  :custom ((idle-update-delay 0.1)        ; <30.1
           (which-func-update-delay 0.1)) ; >=30.1
  :config (which-function-mode))

(use-package which-key
  :demand t
  :commands (which-key-mode)
  :config (which-key-mode))

;;;;; Mode-line

(use-package minions
  :demand t
  :commands (minions-mode)
  :custom (minions-prominent-modes
           '(flycheck-mode flymake-mode lsp-mode))
  :config (minions-mode))

(use-package mode-line
  :ensure nil ; built-in
  :custom ((column-number-mode t)
           (line-number-mode t)
           (mode-line-position-column-line-format '(" (L%l, C%C)"))
           (size-indication-mode t)))

;;;; Xref

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

;;;; YASnippet

(use-package yasnippet
  ;; LSP snippets are handled via YASnippet.
  :hook ((eglot-managed-mode lsp-mode) . yas-minor-mode))

(use-package gpr-yasnippets)

;;; init.el ends here
