;;; projectile-direnv.el --- Set environment variables from .envrc -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Christian Romney <crommney@pointslope.com>
;;         Claude Tete <claude.tete@gmail.com>
;; URL: https://github.com/christianromney/projectile-direnv
;; Keywords: convenience
;; Version: 0.2.0
;; Package-Requires: ((emacs "24") (s "1.11.0") (dash "2.12.0") (projectile "0.13.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I want to launch Emacs as a GUI and have it automatically
;; set the same environment variables that direnv sets for me
;; at the shell when I am in a Projectile project.
;; Capability to set environment variables as buffer-local.
;;
;; See README.org for more info.
;;
;;; Code:

(require 's)
(require 'dash)
(require 'projectile)

(defgroup projectile-direnv nil
  "Use direnv config from projetile root."
  :group 'convenience
  :prefix "projectile-direnv-")

(defcustom projectile-direnv-exec-path-mandatory '()
  "List of mandatory element of exec-path."
  :type 'list)

(defcustom projectile-direnv-make-local nil
  "When non-nil exported variables are buffer-local."
  :type 'boolean)

(defvar projectile-direnv-envrc nil
  "Contains the path to the last loaded .envrc.")

(defun projectile-direnv-parse-export (line)
  "Parses a single line of the form export VAR=VAL into a cons
cell where the car is the var name and the cdr is its value."
  (let* ((parts (s-split "=" line))
         (varname (car (last (s-split " " (car parts)))))
         (varval (car (last parts))))
    (cons varname varval)))

(defun projectile-direnv-read-file-as-string (filename)
  "Returns a the file's contents as a string"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun projectile-direnv-set-env-var (pair)
  "Sets an environment variable. Expects a pair of (VARNAME . VALUE)"
  (let ((name (car pair))
         (value (cdr pair)))
    (when projectile-direnv-make-local
      (projectile-direnv-delete-env-var name))
    (setenv name value)
    (when (string-equal name "PATH")
      (let ((separator (if (string-equal system-type "windows-nt") ";" ":")))
        (setq exec-path (append (s-split separator value t) projectile-direnv-exec-path-mandatory))))))

(defun projectile-direnv-delete-env-var (name)
  "Delete one element into process-environment"
  (setq process-environment
    (cl-remove-if
      (lambda (element)
        (string-match (concat "^" name "=") element))
      process-environment)))

(defun projectile-direnv-list-exports (exports)
  "Returns a string of the form '+VAR1 +VAR2' listing the exported envvars"
  (s-join " " (-map (lambda (e) (s-append (car e) "+")) exports)))

(defun projectile-direnv-export-variable-from-dir (dir)
  "Reads a .envrc file in the DIR, and sets environment variables for any
defined exports, when `projectile-direnv-make-local' is non-nil always read
file and set as buffer-local."
  (let ((envrc (expand-file-name ".envrc" dir)))
    (when (and (file-exists-p envrc)
            (or projectile-direnv-make-local
              (not (string= envrc projectile-direnv-envrc))))
      (let* ((contents (projectile-direnv-read-file-as-string envrc))
              (lines (s-split "\n" contents))
              (exports (-filter (lambda (l) (s-starts-with? "export" l)) lines))
              (envvars (-map #'projectile-direnv-parse-export exports)))
        (when projectile-direnv-make-local
          (make-local-variable 'process-environment)
          (make-local-variable 'exec-path))
        (setq projectile-direnv-envrc envrc)
        (-each envvars #'projectile-direnv-set-env-var)
        (message "projectile-direnv: export %s" (projectile-direnv-list-exports envvars))))))

;;;###autoload
(defun projectile-direnv-export-variables ()
  "Reads a .envrc file in the Projectile project root, and sets
environment variables for any defined exports"
  (interactive)
  (when (projectile-project-p)
    (projectile-direnv-export-variable-from-dir (projectile-project-root))))

(define-minor-mode projectile-direnv-mode
    "Projectile direnv minor mode"
    ;; If init-value is not set to t, this mode does not get enabled in
    ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
    ;; More info: http://emacs.stackexchange.com/q/16693/115
    :init-value t
   )

;; global mode is here to replace hook on `projectile-mode-hook'
;; see https://github.com/bbatsov/projectile/commit/6986e435e64e2f4ee43bf8093c11e1da5fb266fa
;;;###autoload
(define-globalized-minor-mode projectile-direnv-global-mode
  projectile-direnv-mode
  projectile-direnv-export-variables)

(provide 'projectile-direnv)
;;; projectile-direnv.el ends here
