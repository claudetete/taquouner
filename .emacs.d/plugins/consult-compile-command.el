;;; consult-shell-command.el --- Select a compile command with completing-read

;; Copyright (C) 2023-2024 Claude Tete

;; Author: Claude Tete <claude.tete@gmail.com>
;; URL: https://github.com/claudetete/taquouner
;; Version: 0.2
;; Package-Requires: ((consult))
;; Keywords: compile consult shell

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
;; `consult-compile-command' show `completing-read' to select a compile command.

;;; Code:

(defgroup consult-compile-command nil
  "Select a compile command with completing-read."
  :group 'convenience)

(defcustom consult-compile-command-sources 'consult-shell-command--source
  "Function which return sources to use for compile command."
  :type 'function
  :group 'consult-compile-command)

(defcustom consult-compile-command-source-list '("make" "make all" "make clean")
  "A list of compile command.
Can be set in .dir-locals.el file or any project manager."
  :type '(repeat string)
  :group 'consult-compile-command)

(defcustom consult-compile-command-command-prefixs 'consult-compile-command--command-prefix
  "Function which return sources to use for compile command."
  :type 'function
  :group 'consult-compile-command)

(defcustom consult-compile-command-command-prefix nil
  "The prefix to add to each command when calling `compile'"
  :type 'string
  :group 'consult-compile-command)

(defcustom consult-compile-command-command-hook-after-selection 'consult-compile-command--command-hook-after-selection
  "Function which return the command selected modify or not."
  :type 'function
  :group 'consult-compile-command)

(defvar consult--compile-command-history nil)

(defun consult-compile-command--source ()
  "Collect candidates for compile command."
  consult-compile-command-source-list)

(defun consult-compile-command--command-prefix ()
  "Retrieve command prefix for compile command."
  consult-compile-command-command-prefix)

(defun consult-compile-command--command-hook-after-selection (param command)
  "Hook to modify the selected command before running compile command."
  command)

;;;###autoload
(defun consult-compile-command (param)
  "Show a list of command to run with compilation mode using a prefix command
retrieves through `consult-compile-command-command-prefixs' function name."
  (interactive)
  (let* ((prefix (funcall consult-compile-command-command-prefixs param))
         (command-selected (consult--read
                            (funcall consult-compile-command-sources param)
                            :prompt "Compile command: "
                            :category 'consult-compile-command
                            :require-match nil
                            :history '(:input consult--compile-command-history)))
         (command (funcall consult-compile-command-command-hook-after-selection param command-selected)))
    (when prefix
      (setq command (concat prefix command)))
    (compile command)))


(provide 'consult-compile-command)

;;; consult-compile-command.el ends here
