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
;; Version: 1.5
;; Created: June 2012
;; Last-Updated: May 2013

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
;;   ;; edit config spec
;;   (global-set-key    (kbd "C-c c s")         'clearcase-config-spec-edit)
;;   ;; open dired for file version
;;   (global-set-key    (kbd "C-c c o")         'clearcase-dired-file-version)


;;; Change Log:
;; 2013-05-07 (1.5)
;;    add hook when quit config spec mode
;; 2013-04-12 (1.4)
;;    edit of config spec (inspired by clearcase.el) + config spec mode for
;;    color syntax
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
(require 'font-lock)     ;; for syntax color

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

;;; open version file
(defun clearcase-dired-file-version ()
  "Version file of the current buffer."
  (interactive)
  ;; get full path of current buffer
  (let ((my-buffer (concat (clearcase-get-buffer-file-name) "@@/main")))
    ;; call clearcase describe
    (dired my-buffer)))

(defun clearcase-config-spec-edit ()
  "Edit the config spec of the current view."
  (interactive)
  (setq clearcase-buffer-file-name buffer-file-name)
  (let ((cs-buffer-name (concat "*" clearcase-config-spec-file "*"))
         (config-spec (clearcase-config-spec-get)))
    (when config-spec
      (setq clearcase-parent-buffer (current-buffer))
      (kill-buffer (get-buffer-create cs-buffer-name))
      (pop-to-buffer (get-buffer-create cs-buffer-name))
      (auto-save-mode auto-save-default)
      (erase-buffer)
      (insert (clearcase-config-spec-get))
      (goto-char (point-min))
      (re-search-forward "^[^#\n]" nil 'end)
      (beginning-of-line)
      (clearcase-config-spec-mode)
)))

;;
;;;
;;;; CONFIG SPEC MODE
(defvar clearcase-config-spec-file "clearcase-config-spec"
  "Name of the file.")

(defvar clearcase-buffer-file-name nil
  "Name of the current buffer.")

(defvar clearcase-config-spec-mode-hook nil
  "Run after loading clearcase-config-spec-mode.")

(defvar clearcase-config-spec-quit-hook nil
  "Run after quit clearcase-config-spec-mode.")

(defvar clearcase-config-spec-map nil
  "Bind map for config spec mode.")
(if clearcase-config-spec-map
  nil
  (progn
    (setq clearcase-config-spec-map (make-sparse-keymap))
    (define-key clearcase-config-spec-map (kbd "C-c C-c") 'clearcase-config-spec-save-and-quit)
    (define-key clearcase-config-spec-map (kbd "C-x C-s") 'clearcase-config-spec-save)))

(defconst clearcase-config-spec-font-lock-keyword
  (list
    ;; standard rules
    ;; element -file /toto/.../* TOTO -time 02-27-12
    '("^[ \t]*\\(\\<element\\>\\)[ \t]+\\(\\<-dir\\|-directory\\|-file\\|-eltype\\>\\).*[ \t]+\\([^ ]+\\)[ \t]+\\(\\<-mkbranch\\|-nocheckout\\|-time\\>\\)"
       (1 font-lock-keyword-face nil t) (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t) (4 font-lock-keyword-face nil t))
    ;; element -file /toto/.../* TOTO
    '("^[ \t]*\\(\\<element\\>\\)[ \t]+\\(\\<-dir\\|-directory\\|-file\\|-eltype\\>\\).*[ \t]+\\([^ \t\n]+\\)"
       (1 font-lock-keyword-face nil t) (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))
    ;; element * TOTO -time 02-27-12
    '("^[ \t]*\\(\\<element\\>\\).*[ \t]+\\([^ ]+\\)[ \t]+\\(\\<-mkbranch\\|-nocheckout\\|-time\\>\\)"
       (1 font-lock-keyword-face nil t) (2 font-lock-variable-name-face nil t)
       (3 font-lock-keyword-face nil t))
    ;; element * TOTO
    '("^[ \t]*\\(\\<element\\>\\).*[ \t]+\\([^ \t\n]*\\)"
       (1 font-lock-keyword-face nil t) (2 font-lock-variable-name-face nil t))
    ;; mkbranch
    '("^[ \t]*\\(\\<end\\>\\)?[ \t]+\\<mkbranch\\>" . font-lock-keyword-face)
    ;; time
    '("^[ \t]*\\(\\<end\\>\\)?[ \t]+\\<time\\>" . font-lock-keyword-face)
    ;; include
    '("^[ \t]*\\<include\\>" . font-lock-keyword-face)
    )
  "Keyword for clearcase-config-spec-mode.")

(setq auto-mode-alist
  (append
    '(("\\.cs\\'" . clearcase-config-spec-mode))
    auto-mode-alist))

(defvar clearcase-config-spec-syntax-table nil
  "Syntax table for clearcase-config-spec-mode.")

(defun clearcase-config-spec-create-syntax-table ()
  (if clearcase-config-spec-syntax-table
    ()
    (setq clearcase-config-spec-syntax-table (make-syntax-table))
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" clearcase-config-spec-syntax-table)
    ; comment start with # and end with \n
    (modify-syntax-entry ?# "< b" clearcase-config-spec-syntax-table)
    (modify-syntax-entry ?\n "> b" clearcase-config-spec-syntax-table))
  (set-syntax-table clearcase-config-spec-syntax-table)
  )

(defun clearcase-config-spec-mode ()
  "Edit config spec mode."
  (interactive)
  (kill-all-local-variables)

  ;; syntax
;;  (setq comment-start "#")
  (clearcase-config-spec-create-syntax-table)

  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(clearcase-config-spec-font-lock-keyword nil t))

  (use-local-map clearcase-config-spec-map)
  (setq major-mode 'clearcase-config-spec-mode)
  (setq mode-name "config-spec")
  (make-variable-buffer-local 'clearcase-parent-buffer)
  (set-buffer-modified-p nil)
  (run-hooks 'text-mode-hook 'clearcase-config-spec-mode-hook))

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

(defun clearcase-config-spec-get ()
  "Return the config spec of the current view when in a clearcase view otherwise
nil."
  ;; get the default directory
  (let* (ret (path (file-name-directory clearcase-buffer-file-name)))
    ;; the path is from a mounted view
    (if (clearcase-in-a-clearcase-view path)
      (progn
        (setq default-directory path)
        ;; get the config spec
        (setq ret (shell-command-to-string "cleartool catcs")))
      (progn
        (setq ret nil)
        (message (concat "This directory is not part of a clearcase view: " path))))
    ret))

(defun clearcase-config-spec-save ()
  "Apply the config spec and save it."
  (interactive)
  (if (not (buffer-modified-p))
    (message "Config spec has not changed since last saved")
    (progn
      (message "Setting config spec...")
      ;; write config spec in a file
      (write-region (point-min) (point-max) clearcase-config-spec-file 0)
      ;; set config spec
      (shell-command-to-string (concat "cleartool setcs " clearcase-config-spec-file))
      (set-buffer-modified-p nil)
      (message "Setting config spec...done")
      )
    )
  )

(defun clearcase-config-spec-save-and-quit ()
  "Apply the config spec and kill the buffer."
  (interactive)
  (let ((my-buffer (current-buffer)))
    ;; set config spec
    (clearcase-config-spec-save)
    (if (file-exists-p clearcase-config-spec-file)
      ;; delete temp file
      (delete-file clearcase-config-spec-file))
    (bury-buffer nil)
    (kill-buffer my-buffer))
  (run-hooks 'clearcase-config-spec-quit-hook))


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
