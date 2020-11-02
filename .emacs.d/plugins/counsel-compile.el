;;; counsel-compile.el --- Select a compile command with ivy

;; Copyright (C) 2020 Claude Tete

;; Author: Claude Tete <claude.tete@gmail.com>
;; URL: https://github.com/claudetete/taquouner
;; Version: 0.1
;; Package-Requires: ((ivy))
;; Keywords: compile

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `counsel-compile' show `ivy' to select a compile command.

;;; Code:

(defgroup counsel-compile nil
  "Select a compile command with ivy."
  :group 'convenience)

(defcustom counsel-compile-sources 'counsel-compile--source
  "Function which return sources to use for compile command."
  :type 'function
  :group 'counsel-compile)

(defcustom counsel-compile-source-list '("make" "make all" "make clean")
  "A list of compile command.
Can be set in .dir-locals.el file or any project manager."
  :type '(repeat string)
  :group 'counsel-compile)

(defcustom counsel-compile-command-prefixs 'counsel-compile--command-prefix
  "Function which return sources to use for compile command."
  :type 'function
  :group 'counsel-compile)

(defcustom counsel-compile-command-prefix nil
  "The prefix to add to each command when calling `compile'"
  :type 'string
  :group 'counsel-compile)

(defun counsel-compile--source ()
  "Collect candidates for compile command."
  counsel-compile-source-list)

(defun counsel-compile--command-prefix ()
  "Retrieve command prefix for compile command."
  counsel-compile-command-prefix)

;;;###autoload
(defun counsel-compile ()
  "Show a list of command to run with compilation mode using a prefix command
retrieves through `counsel-compile-command-prefixs' function name."
  (interactive)
  (let ((candidates (funcall counsel-compile-sources)))
    (ivy-read "Compile command: " candidates
      :action (lambda (command)
                (let ((prefix (funcall counsel-compile-command-prefixs)))
                  (when prefix
                    (setq command (concat prefix " " command)))
                  (compile command)))
      :caller 'counsel-compile)))


(provide 'counsel-compile)

;;; counsel-compile.el ends here
