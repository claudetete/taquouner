;;; 02-mode-061-magit.el --- configuration of magit mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.4
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.use git with nice interface (do not use vc interface from emacs)]
;; [SUBDEFAULT.nil]
;;


;;; Code:
;;(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/magit-master/lisp"))
(use-package magit
  :bind
  ;; git status (entry point of magit) then 'h' to popup help
  ("C-c g g"   . magit-status)
  ;; git fetch
  ("C-c g f"   . magit-fetch-popup)
  ;; git stash
  ("C-c g s"   . magit-stash-popup)
  ;; git push
  ("C-c g p"   . magit-push-popup)
  ;; git pull
  ("C-c g u"   . magit-pull-popup)
  ;; git merge
  ("C-c g m"   . magit-merge-popup)
  ;; magit dispatch
  ("C-c g SPC" . magit-dispatch-popup)
  ;; magit diff
  ("C-c g ="   . magit-difftool-buffer-file)
  ;; magit file/buffer
  ("C-c g b"   . magit-file-popup)
  ;; magit fil history with external tool
  ("C-c g l"   . magit-file-history-external)
  ("C-c g h"   . magit-file-history-external)

  :config
  (setq magit-commit-all-when-nothing-staged t)
  (setq magit-process-popup-time 1)

  ;; do not show diff when commit
  (setq magit-commit-show-diff nil)

  (defun magit-difftool-buffer-file ()
    "Open git difftool to compare with HEAD (no commited changes)."
    (interactive)
    (magit-with-toplevel
      (-if-let (file (magit-file-relative-name))
        (magit-run-git-async "difftool" "--gui" "--no-prompt" "HEAD" "--" file)
        (user-error "Buffer isn't visiting a file"))))

  (defun magit-file-history-external ()
    "Open GUI of git filehistory (need to have GitExtensions in PATH).
Useful until magit can open difftool instead of Ediff."
    (interactive)
    ;; get absolute file path of current file
    (magit-with-toplevel
      (-if-let (file (concat (file-name-as-directory (magit-toplevel)) (magit-file-relative-name)))
        (magit-run-async-command (concat "GitExtensions.exe filehistory " file) t)
        (user-error "Buffer isn't visiting a file"))))

  (defvar magit-async-command-buffer "*magit async*")
  ;; run an asynchronous command to call command
  (defun magit-run-async-command (command &optional toplevel keep-buffer)
    "Call the COMMAND in an asynchronous process."
    ;; do not show a new window
    (save-window-excursion
      (if keep-buffer
        (progn
          (kill-buffer (get-buffer-create magit-async-command-buffer))
          ;; get current buffer
          (let ((my-async-buffer (generate-new-buffer magit-async-command-buffer))
                 (default-directory (if toplevel
                                      (or (magit-toplevel)
                                        (user-error "Not inside a Git repository"))
                                      default-directory)))
            ;; call command
            (async-shell-command command my-async-buffer)
            ;; do not ask about process running with async buffer
            (set-process-query-on-exit-flag (get-buffer-process my-async-buffer) nil)
            ;; pop async buffer (after 1s: to wait async process start)
            (run-with-timer 1 nil (lambda (my-async-buffer) (pop-to-buffer magit-async-command-buffer)) my-async-buffer)))
        (progn
          (set (make-local-variable 'async-shell-command-display-buffer) nil)
          ;; call command
          (async-shell-command command))
        )))

  ;; overload magit ediff compare function to run difftool
  (defun magit-ediff-compare (revA revB fileA fileB)
    "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range)."
    (interactive
      (pcase-let ((`(,revA ,revB) (magit-ediff-compare--read-revisions
                                    nil current-prefix-arg)))
        (nconc (list revA revB)
          (magit-ediff-read-files revA revB))))
    ;; (message (concat "magit-ediff-compare: " fileA "=" revA ", " fileB "=" revB))
    (message (concat "git difftool --gui --no-prompt -M -C \"" revA "\" \"" revB "\" -- \"" fileA "\""))
    (magit-run-git-async
      "difftool" "--gui" "--no-prompt" "--find-renames" "--find-copies" revA revB "--" fileA))

  ;; overload ediff stage file to run difftool
  (defun magit-ediff-stage (file)
    "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository."
    (interactive
      (list (magit-completing-read "Selectively stage file"
              (magit-tracked-files) nil nil nil nil
              (magit-current-file))))
    (message "%s %s %s %s %s %s" "git" "difftool" "--gui" "--no-prompt" "--" file)
    (magit-run-git-async
      "difftool" "--gui" "--no-prompt" "--" file))
  ) ;; (use-package magit


(provide '02-mode-061-magit)

;;; 02-mode-061-magit.el ends here2
