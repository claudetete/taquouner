;;; ack-emacs.el --- a ack mode to replace grep

;; Copyright (C) 2008 Kim van Wyk and Johan Kohler
;; for original mode
;; With credit to w32-find-dired.el

;; Copyright (C) 2009-2011 Nikolaj Schumacher
;; for type-alist

;; Copyright (C) 2012-2013 Claude Tete <claude.tete@gmail.com>
;; for ack-same() (inspired by ack-and-a-half)
;; and face from grep-mode

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Keywords: ack, grep, mode
;; Author: Kim van Wyk and Johan Kohler
;;         Nikolaj Schumacher
;;         Claude Tete  <claude.tete@gmail.com>
;; Version: 1.2
;; Created: 2008
;; Last-Updated: May 2013

;;; Commentary:
;;
;; Bug when you put a parenthesis in pattern, like "(?<=foo)bar" (on MS Windows):
;;   /usr/bin/bash: -c: line 0: syntax error near unexpected token `)'
;; Same error when I try with `shell-command' ...

;;; Change Log:
;; 2013-05-17 (1.2)
;;    update for ack 2.0x
;; 2012-12-27 (1.1)
;;    fix double quote bug
;; 2012-11-27 (1.0)
;;    add color from grep + ack() to ack-dir() and add ack() with relative path
;;    by default
;; 2012-11-26 (0.9)
;;    creation from initial mode + add ack-same()


;;; Code:

(require 'compile)
(require 'grep)
(require 'thingatpt)

(defvar ack-command "ack" "The command run by the ack function.")

(defvar ack-mode-font-lock-keywords
   '(;; Command output lines.
     ("^.+?-[0-9]+-.*\n" (0 grep-context-face))
     )
   "Additional things to highlight in ack output.
This gets tacked on the end of the generated expressions.")

(defvar ack-use-search-in-buffer-name t
  "If non-nil, use the search string in the ack buffer's name.")

(defconst ack-mode-type-alist
  '(
     (actionscript-mode "actionscript")
     (LaTeX-mode "tex")
     (TeX-mode "tex")
     (asm-mode "asm")
     (batch-file-mode "batch")
     (c++-mode "cpp")
     (c-mode "cc")
     (cfmx-mode "cfmx")
     (cperl-mode "perl")
     (csharp-mode "csharp")
     (css-mode "css")
     (emacs-lisp-mode "elisp")
     (erlang-mode "erlang")
     (espresso-mode "java")
     (fortran-mode "fortran")
     (haskell-mode "haskell")
     (hexl-mode "binary")
     (html-mode "html")
     (java-mode "java")
     (javascript-mode "js")
     (jde-mode "java")
     (js2-mode "js")
     (jsp-mode "jsp")
     (latex-mode "tex")
     (lisp-mode "lisp")
     (lua-mode "lua")
     (makefile-mode "make")
     (mason-mode "mason")
     (nxml-mode "xml")
     (objc-mode "objc" "objcpp")
     (ocaml-mode "ocaml")
     (parrot-mode "parrot")
     (perl-mode "perl")
     (php-mode "php")
     (plone-mode "plone")
     (python-mode "python")
     (ruby-mode "ruby")
     (scala-mode "scala")
     (scheme-mode "scheme")
     (shell-script-mode "shell")
     (skipped-mode "skipped")
     (smalltalk-mode "smalltalk")
     (sql-mode "sql")
     (tcl-mode "tcl")
     (tex-mode "tex")
     (tt-mode "tt")
     (vb-mode "vb")
     (vim-mode "vim")
     (xml-mode "xml")
     (yaml-mode "yaml"))
  "*File type(s) to search per major mode. (ack-same)
The car in each list element is a major mode, and the rest
is a list of strings passed to the --type flag of ack when running
`ack-same'.")

(define-compilation-mode ack-mode "Ack"
  "Specialization of compilation-mode for use with ack."
  (set (make-local-variable 'compilation-error-face)
    grep-hit-face)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns)
       grep-error-screen-columns)
  (add-hook 'compilation-filter-hook 'ack-filter nil t)
  )

(defun ack-filter ()
  "Handle match highlighting escape sequences inserted by the ack process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight ack matches and delete marking sequences.
        (while (re-search-forward "\033\\[31m\\(.*?\\)\033\\[0m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face grep-match-face)
                         t t))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(defun ack-run (dir pattern args)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
 While ack runs asynchronously, you can use the \\[next-error] command to
 find the text that ack hits refer to. The command actually run is
 defined by the ack-command variable."
  (if (not (string-prefix-p "." dir))
    ;; Get dir into an the right state, incase a file name was used
    (setq dir (abbreviate-file-name
                (file-name-as-directory (expand-file-name dir)))))
  ;; Check that it's really a directory.
  (or (file-directory-p dir)
    (error "ack needs a directory: %s" dir))
  ;; run ack
  (let (compile-command
         (compilation-directory default-directory)
         (ack-full-buffer-name "*ack*"))
    ;; (save-some-buffers (not compilation-ask-about-save) nil)
    ;; lambda defined here since compilation-start expects to call a function to get the buffer name
    (compilation-start (concat ack-command " --nofilter --color --color-filename=magenta --color-match=red --color-lineno=green " args " " pattern " " dir) 'ack-mode
      (when ack-use-search-in-buffer-name
        (function (lambda (ignore)
                    ack-full-buffer-name)))
      (regexp-quote pattern))))

(defun ack-type ()
  "Return --type option or --type-set"
  (let ((type (car (cdr (assq major-mode ack-mode-type-alist))))
         (ext (car (last (split-string buffer-file-name "\\."))))
         (mode (car (split-string (format "%s" major-mode) "-"))))
    (if type
      ;; known type
      (concat "--" type)
      ;; unknown type use the file extension
      (concat "--type-set " mode ":ext:" ext " --" mode))))

(defun ack (dir pattern args)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
 While ack runs asynchronously, you can use the \\[next-error] command to
 find the text that ack hits refer to. The command actually run is
 defined by the ack-command variable."
  (interactive (list (let ((insert-default-directory nil))
                       (read-file-name "Run ack in directory (put . to have relative path): " nil "" t ""))
                 (read-string "Search for: " (thing-at-point 'symbol))
                 (read-string "Ack arguments: " "--nogroup -Q -i " nil "" nil)))
  ;; run ack
  (ack-run dir pattern args))

(defun ack-dir (dir pattern args)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
 While ack runs asynchronously, you can use the \\[next-error] command to
 find the text that ack hits refer to. The command actually run is
 defined by the ack-command variable."
  (interactive (list (read-file-name "Run ack in directory: " nil "" t)
                 (read-string "Search for: " (thing-at-point 'symbol))
                 (read-string "Ack arguments: " "--nogroup -Q -i " nil "" nil)))
  ;; run ack
  (ack-run dir pattern args))

(defun ack-same (dir pattern args)
  "Run ack with --type matching the current `major-mode'.
The types of files searched are determined by `ack-mode-type-alist' and
`ack-mode-extension-alist'.  If no type is configured, the buffer's
file extension is used for the search."
  (interactive (list (let ((insert-default-directory nil))
                       (read-file-name "Run same ack in directory (put . to have relative path): " nil "" t))
                 (read-string "Search for: " (thing-at-point 'symbol))
                 ;; get type
                 (read-string "Ack arguments: " (concat "--nogroup " (ack-type) " -Q -i") nil "" nil)))
  ;; run ack
  (ack-run dir pattern args))


(provide 'ack-emacs)
