;;; helm-compile.el --- Select a compile command with helm

;; Copyright (C) 2020 Claude Tete

;; Author: Claude Tete <claude.tete@gmail.com>
;; URL: https://github.com/claudetete/taquouner
;; Version: 0.1
;; Package-Requires: ((helm-core))
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
;; `helm-compile' show `helm' to select a compile command.

;;; Code:

(defgroup helm-compile nil
  "Select a compile command with helm."
  :group 'convenience)

(defcustom helm-compile-sources 'helm-compile--source
  "Function which return sources to use for compile command."
  :type 'function
  :group 'helm-compile)

(defcustom helm-compile-source-list '("make" "make all" "make clean")
  "A list of compile command.
Can be set in .dir-locals.el file or any project manager."
  :type '(repeat string)
  :group 'helm-compile)

(defcustom helm-compile-command-prefixs 'helm-compile--command-prefix
  "Function which return sources to use for compile command."
  :type 'function
  :group 'helm-compile)

(defcustom helm-compile-command-prefix nil
  "The prefix to add to each command when calling `compile'"
  :type 'string
  :group 'helm-compile)

(defvar helm-compile-source
  (helm-build-sync-source "Compile"
    :candidates (lambda () (funcall helm-compile-sources))
    :action '(("Compile command" .
                (lambda (command)
                  (let ((prefix (funcall helm-compile-command-prefixs)))
                    (when prefix
                      (setq command (concat prefix " " command)))
                    (compile command))))))
  "Helm source for compile command.")

(defun helm-compile--source ()
  "Collect candidates for compile command."
  helm-compile-source-list)

(defun helm-compile--command-prefix ()
  "Retrieve command prefix for compile command."
  helm-compile-command-prefix)

(defun helm-compile ()
  (interactive)
  (helm
    :sources helm-compile-source
    :buffer "*helm compile*"))


(provide 'helm-compile)

;;; helm-compile.el ends here
