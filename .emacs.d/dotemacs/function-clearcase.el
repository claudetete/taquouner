;;; function-clearcase.el --- some function for clearcase integration

;; Copyright (c) 2012-2013 Claude Tete
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

;; Keywords: config, function, clearcase
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: June 2012
;; Last-Updated: January 2013

;;; Commentary:
;;
;; load by `functions.el'
;;
;; under MS Windows can be used with AutoHotKey script ClearCaseShortcut.ahk to
;; keep homogeneous bind for clearcase functions
;;
;; it can be combined with clearcase.el minor mode
;;
;; Example of bind in emacs:
;;   ;; checkout
;;   (global-set-key    (kbd "C-c c c")         'clearcase-gui-checkout)
;;   ;; diff
;;   (global-set-key    (kbd "C-c c =")         'clearcase-gui-diff-prev)
;;   ;; history
;;   (global-set-key    (kbd "C-c c h")         'clearcase-gui-history)
;;   ;; uncheckout
;;   (global-set-key    (kbd "C-c c u")         'clearcase-gui-uncheckout)
;;   ;; version tree
;;   (global-set-key    (kbd "C-c c t")         'clearcase-gui-version-tree)
;;   ;; clearcase explorer
;;   (global-set-key    (kbd "C-c c e")         'clearcase-gui-explorer)
;;   ;; version properties
;;   (global-set-key    (kbd "C-c c v")         'clearcase-gui-version-properties)
;;   ;; element properties
;;   (global-set-key    (kbd "C-c c p")         'clearcase-gui-properties)
;;   ;; element properties
;;   (global-set-key    (kbd "C-c c i")         'clearcase-gui-checkin)
;;   ;; element properties
;;   (global-set-key    (kbd "C-c c f")         'clearcase-gui-find-checkout)


;;; Change Log:
;; 2013-01-31 (1.3)
;;    change test clearcase file + remove async empty buffer + function name
;;    match clearcase.el
;; 2012-06-27 (1.2)
;;    fix bug with directory name and find checkout
;; 2012-06-21 (1.1)
;;    improvement + clean up
;; 2012-06-20 (1.0)
;;    split from functions.el


;;; Code:
;;
;;;
;;;; FUNCTION (interactive)
;;; Checkout file from view in clearcase
(defun clearcase-gui-checkout ()
  "Checkout the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase checkout
    (clearcase-run-async-command my-buffer "cleardlg /window 5061e /windowmsg A065 /checkout")))

;;; Checkin file from view in clearcase
(defun clearcase-gui-checkin ()
  "Checkin the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase describe
    (clearcase-run-async-command my-buffer "cleardlg /window 606f6 /windowmsg A065 /checkin")))

;;; Undo Checkout file from view in clearcase
(defun clearcase-gui-uncheckout ()
  "Uncheckout the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase uncheckout
    (clearcase-run-async-command my-buffer "cleardlg /window c04ca /windowmsg A065 /uncheckout")))

;;; Graphical history of current file
(defun clearcase-gui-history ()
  "Show history of the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase history
    (clearcase-run-async-command my-buffer "clearhistory")))

;;; Graphical diff file with its predecessor
(defun clearcase-gui-diff-prev ()
  "Show diff with the current buffer and its predecessor graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase diff
    (clearcase-run-async-command my-buffer "cleartool diff -graphical -predecessor")))

;;; Version Tree file from view in clearcase
(defun clearcase-gui-version-tree ()
  "Version tree of the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase version tree
    (clearcase-run-async-command my-buffer "clearvtree")))

;;; ClearCase explorer of file from view in clearcase
(defun clearcase-gui-explorer ()
  "ClearCase Explorer of the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase explorer
    (clearcase-run-async-command my-buffer "clearexplorer")))

;;; Find checkout files from view in clearcase
(defun clearcase-gui-find-checkout ()
  "Find checkout for the current directory graphically."
  (interactive)
  ;; get full path of current dir
  (let ((my-dir (clearcase-get-buffer-directory-name)))
    ;; call clearcase describe
    (clearcase-run-async-command my-dir "clearfindco")))

;;; Properties of file from view in clearcase
(defun clearcase-gui-properties ()
  "Properties of the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase describe
    (clearcase-run-async-command (concat my-buffer "@@") "cleardescribe")))

;;; Version properties of file from view in clearcase
(defun clearcase-gui-version-properties ()
  "Version properties of the current buffer graphically."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (clearcase-get-buffer-file-name)))
    ;; call clearcase describe
    (clearcase-run-async-command my-buffer "cleardescribe")))

;;
;;;
;;;; FUNCTION (elisp)
;;; get the buffer file name and check it
(defun clearcase-get-buffer-file-name ()
  "Return the file name or nil if current buffer is not in clearcase view or if
it is a view private file."
  (let* (ret (my-buffer buffer-file-name))
    ;; the path is from a mounted view
    (if (clearcase-in-a-clearcase-view (file-name-directory my-buffer))
      (progn
        ;; set in windows path when in MS Windows
        (when (string-equal system-type "windows-nt")
          (setq my-buffer (replace-regexp-in-string "/" "\\\\" my-buffer)))
        ;; return the buffer file name or nil when view private file
        (setq ret (clearcase-is-not-a-private-object my-buffer)))
      ;; the current file is not from clearcase
      (progn
        (message (concat "This file is not part of a clearcase view: " buffer-file-name))
        (setq ret nil)))
    ret)) ; return value

;;; get the buffer directory name and check it
(defun clearcase-get-buffer-directory-name ()
  "Return the directory name or nil if current directory is not in clearcase
view or if it is a view private directory."
  (let* (ret (my-dir (file-name-directory buffer-file-name)))
    ;; the path is from a mounted view
    (if (clearcase-in-a-clearcase-view my-dir)
      (progn
        ;; set in windows path when in MS Windows
        (when (string-equal system-type "windows-nt")
          (setq my-dir (replace-regexp-in-string "/" "\\\\" my-dir)))
        ;; return the buffer file name or nil when view private file
        (setq ret (clearcase-is-not-a-private-object my-dir)))
      ;; the current file is not from clearcase
      (progn
        (message (concat "This directory is not part of a clearcase view: " my-dir))
        (setq ret nil)))
    ret)) ; return value

;;; test when directory is from clearcase
(defun clearcase-in-a-clearcase-view (path)
  "Return the PATH of the object when it is in a clearcase view otherwise nil."
  ;; get the default directory
  (let* (ret)
    ;; set current directory
    (setq default-directory path)
    ;; test when cleartool return "** NONE **"
    (if (string-match "\\*\\* NONE \\*\\*" (shell-command-to-string "cleartool pwv -wdview -short"))
      nil
      t)))

;;; test when directory is from clearcase
(defun clearcase-is-not-a-private-object (path)
  "Return the PATH of the object when it is not a view private object otherwise
nil."
  ;; get the default directory
  (let* (ret)
    ;; test when cleartool return "view private object"
    (if (string-match "view private object" (shell-command-to-string (concat "cleartool describe -fmt \"%m\"" path)))
      (progn
        (setq ret nil)
        (message (concat "This file is a view private object: " path)))
      (setq ret path))
    ret))

;;; run an asynchronous command to call clearcase command
(defun clearcase-run-async-command (path command)
  "Call the COMMAND with PATH as parameter."
  ;; do not show a new window
  (save-window-excursion
    ;; get current buffer
    (let ((my-async-buffer (generate-new-buffer "async")))
      ;; only when the buffer name is valid
      (when path
        ;; call command
        (async-shell-command (concat command " \"" path "\"") my-async-buffer)
        ;; do not ask about process running with async buffer
        (set-process-query-on-exit-flag (get-buffer-process my-async-buffer) nil)
        ;; kill async buffer after 1s
        (run-with-timer 1 nil (lambda (my-async-buffer) (kill-buffer my-async-buffer)) my-async-buffer)))))


(provide 'function-clearcase)

;;; function-clearcase.el ends here
