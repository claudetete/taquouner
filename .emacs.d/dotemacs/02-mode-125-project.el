;;; 02-mode-125-project.el --- configuration of project.el -*- lexical-binding: t -*-

;; Copyright (c) 2026 Claude Tete
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
;; Created: October 2026
;; Last-Updated: October 2026

;;; Commentary:
;;
;; [SUBHEADER.project: mode for managin project, built-in of GNU Emacs]
;; [SUBDEFAULT.t]


;;; Code:
(use-package project
  :ensure nil

  :custom
  ;; add file names defining a root for project.el
  (project-vc-extra-root-markers '(".project"))

  :config
  ;; use fd instead of find to search files of a project (thanks to https://codeberg.org/woolsweater/.emacs.d/src/commit/ce8620337f0d4a6249fcea39a9c4eb4d910d92bc/modules/conjecture.el#L334-L345)
  (cl-defmethod project-files :around (project &optional dirs)
    "Use \\='fd' if available to find all files for PROJECT in DIRS."
    (if-let ((fd (executable-find "fd")))
        (let* ((search-dirs
                (string-join
                 (mapcar (lambda (d) (shell-quote-argument (expand-file-name d)))
                         (or dirs (list (project-root project))))
                 " "))
             (command (format "%s --hidden --type f --print0 '.*' %s" fd search-dirs)))
          (split-string (shell-command-to-string command) "\0" t))
      (cl-call-next-method project dirs)))

  )

(provide '02-mode-125-project)

;;; 02-mode-125-project.el ends here
