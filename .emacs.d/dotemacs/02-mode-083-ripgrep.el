;;; 02-mode-083-ripgrep.el --- configuration of ripgrep mode with helm

;; Copyright (c) 2017-2018 Claude Tete
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
;; Version: 0.2
;; Created: September 2017
;; Last-Updated: January 2018

;;; Commentary:
;;
;; [SUBHEADER.A front-end for rg, ripgrep (faster than anything...)]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2018-01-31 (0.2)
;;    add settings to add type of file in ripgrep because there is no global
;;    setting (like a .ripgreprc)
;; 2017-09-01 (0.1)
;;    creation from scratch


;;; Code:
(when tqnr-section-mode-helm
  (try-require 'autoload-helm-ag "      ")
  (custom-set-variables
    ;; use thing at point to get default value
    '(helm-ag-insert-at-point 'symbol)
    ;; use ripgrep search with helm-ag mode
    ;; [[VARCOMMENT.List of types to add to ripgrep configuration (no .ripgrep configuration file only cli parameters)
    ;; It should respect ripgrep format for --type-add parameter (extract from $ripgrep --help):
    ;;   --type-add <TYPE>...
    ;;   Add a new glob for a particular file type. Only one glob can be added at a time.
    ;;   Multiple --type-add flags can be provided. Unless --type-clear is used, globs are added
    ;;   to any existing globs defined inside of ripgrep.
    ;;
    ;;   Note that this MUST be passed to every invocation of ripgrep. Type settings are NOT
    ;;   persisted.
    ;;
    ;;   Example: rg --type-add 'foo:*.foo' -tfoo PATTERN.
    ;;
    ;;   --type-add can also be used to include rules from other types with the special include
    ;;   directive. The include directive permits specifying one or more other type names
    ;;   (separated by a comma) that have been defined and its rules will automatically be
    ;;   imported into the type specified. For example, to create a type called src that matches
    ;;   C++, Python and Markdown files, one can use:
    ;;
    ;;   --type-add 'src:include:cpp,py,md'
    ;;
    ;;   Additional glob rules can still be added to the src type by using the --type-add flag
    ;;   again:
    ;;
    ;;   --type-add 'src:include:cpp,py,md' --type-add 'src:*.foo'
    ;;
    ;;   Note that type names must consist only of Unicode letters or numbers. Punctuation
    ;;   characters are not allowed.
    ;; ]]
    ;; [[VARIABLE.tqnr-section-mode-ripgrep-additional-type
    ;;   (list
    ;;     ;; example to add all files .ex and .ample for "example" major mode
    ;;     "example:*.ex,*.ample"
    ;;     )
    ;; ]]
    '(helm-ag-base-command (concat "rg --smart-case --no-heading --line-number"
                             (when tqnr-section-mode-ripgrep-additional-type
                               (concat
                                 " --type-add \""
                                 (mapconcat 'identity tqnr-section-mode-ripgrep-additional-type "\" --type-add \"")
                                 "\"")))))


  (with-eval-after-load "ada-mode"
    (setq-local helm-ag-command-option "--type-add ada:*.adb --type-add ada:*.ads --type ada"))


  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      (global-set-key   (kbd "<M-f3>")  'helm-do-ag)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )





(provide '02-mode-083-ripgrep)

;;; 02-mode-083-ripgrep.el ends here
