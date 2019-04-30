;;; 02-mode-061-magit.el --- configuration of magit mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.use git with nice interface (do not use vc interface from emacs)]
;; [SUBDEFAULT.nil]
;;


;;; Code:
;; under windows you can use msys make (after edit of Makefile) to install magit
;; dash is a dependency of projectile
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/magit-master/lisp"))
(when (try-require 'magit "    ")
  (setq magit-commit-all-when-nothing-staged t)
  (setq magit-process-popup-time 1)

  ;; do not show diff when commit
  (setq magit-commit-show-diff nil)

  (defun magit-difftool-buffer-file ()
    "Open git difftool to compare with HEAD (no commited changes)."
    (interactive)
    (-if-let (file (magit-file-relative-name))
      (magit-run-async-command
        (concat "git difftool " (or magit-buffer-refname
                                  (magit-get-current-branch)
                                  "HEAD")
          " --no-prompt -- " file) t t)
      (user-error "Buffer isn't visiting a file")))

  (defun magit-difftool-buffer-file ()
    "Open git difftool to compare with HEAD (no commited changes)."
    (interactive)
    (-if-let (file (magit-file-relative-name))
      (magit-run-async-command (concat "git difftool HEAD --no-prompt -- " file) t nil)
      (user-error "Buffer isn't visiting a file")))

  (defun magit-file-history-external ()
    "Open GUI of git filehistory (need to have GitExtensions in PATH).
Useful until magit can open difftool instead of Ediff."
    (interactive)
    ;; get absolute file path of current file
    (-if-let (file (concat (file-name-as-directory (magit-toplevel)) (magit-file-relative-name)))
      (magit-run-async-command (concat "gitex.cmd filehistory " file) t)
      (user-error "Buffer isn't visiting a file")))

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

  ;; add X shortcut in popup action to open with difftool instead of Ediff
  (try-require 'magit-exttool "      ")

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; git status (entry point of magit) then 'h' to popup help
      (global-set-key   (kbd "C-c g g")         'magit-status)
      ;; git fetch
      (global-set-key   (kbd "C-c g f")         'magit-fetch-popup)
      ;; git stash
      (global-set-key   (kbd "C-c g s")         'magit-stash-popup)
      ;; git push
      (global-set-key   (kbd "C-c g p")         'magit-push-popup)
      ;; git pull
      (global-set-key   (kbd "C-c g u")         'magit-pull-popup)
      ;; git merge
      (global-set-key   (kbd "C-c g m")         'magit-merge-popup)
      ;; magit dispatch
      (global-set-key   (kbd "C-c g SPC")       'magit-dispatch-popup)
      ;; magit diff
      (global-set-key   (kbd "C-c g =")         'magit-difftool-buffer-file)
      ;; magit file/buffer
      (global-set-key   (kbd "C-c g b")         'magit-file-popup)
      ;; magit fil history with external tool
      (global-set-key   (kbd "C-c g l")         'magit-file-history-external)
      (global-set-key   (kbd "C-c g h")         'magit-file-history-external)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )

(provide '02-mode-061-magit)

;;; 02-mode-061-magit.el ends here2
