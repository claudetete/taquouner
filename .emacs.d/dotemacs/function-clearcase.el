;;; function-clearcase.el --- some function for clearcase integration

;; Copyright (c) 2012 Claude Tete
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
;; Version: 1.2
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `functions.el'
;; REQUIREMENT: var     `section-mode-clearcase'
;;              OR
;;              var     `section-mode-vc-clearcase'

;;; Change Log:
;; 2012-06-27 (1.2)
;;    fix bug with directory name and find checkout
;; 2012-06-21 (1.1)
;;    improvement + clean up
;; 2012-06-20 (1.0)
;;    split from functions.el


;;; Code:
;;;;  CONST
(defconst clearcase-mounted-view "MZ"
  "Letters of clearcase mounted view with MS Windows.")

;;;; INTERNAL FUNCTION
;;; get the buffer file name and check it is from a view
(defun clearcase-get-buffer-file-name ()
  "Return nil if current buffer is not in clearcase view otherwise positive."
  (let* (ret (my-buffer "") (private-flag nil) output-list)
    ;; the path is from a mounted view
    (if (string-match (concat
                        "^["
                        (upcase clearcase-mounted-view)
                        (downcase clearcase-mounted-view)
                        "]:")
          buffer-file-name)
      (progn
        ;; set in windows path
        (setq my-buffer (replace-regexp-in-string "/" "\\\\" buffer-file-name))
        ;;
        (save-window-excursion
          ;; get object kind of file
          (setq output-list (process-lines "cleartool" "describe" "-fmt" "\"%m\"" my-buffer))
          ;; check private file
          (when (string= (car output-list) "\"view private object\"")
            (message (concat "Private file: " my-buffer))
            (setq private-flag t))
          )
        ;; set returned value
        (setq ret (list my-buffer private-flag))
        )
      ;; the current file is not from clearcase
      (progn
        (message (concat "This file is not part of a clearcase view: " buffer-file-name))
        (setq ret nil)
        )
      )
    ret ; return value
    )
  )

(defun clearcase-get-buffer-directory-name ()
  "Return nil if current directory is not in clearcase view otherwise positive."
  (let ((ret (clearcase-get-buffer-file-name)))
    (if (and ret (not (nth 1 ret)))
      ;; set in windows path
      (replace-regexp-in-string "\\\\[^\\\\]*$" " " (car ret))
      ;; the current file is not from clearcase
      nil
      )
    )
  )

;; Checkout file from view in clearcase (by Claude TETE)
(defun clearcase-checkout-graphical ()
  "Checkout the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase checkout
        (async-shell-command (concat "cleardlg.exe /window 5061e /windowmsg A065 /checkout \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Graphical diff file with its predecessor (by Claude TETE)
(defun clearcase-diff-graphical ()
  "Show diff with the current buffer and its predecessor if it is from
clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase diff
        (async-shell-command (concat "cleartool.exe diff -graphical -predecessor \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Graphical history of current file (by Claude TETE)
(defun clearcase-history-graphical ()
  "Show history of the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase history
        (async-shell-command (concat "clearhistory.exe \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Undo Checkout file from view in clearcase (by Claude TETE)
(defun clearcase-uncheckout-graphical ()
  "Uncheckout the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get of current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase uncheckout
        (async-shell-command (concat "cleardlg.exe /window c04ca /windowmsg A065 /uncheckout \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Version Tree file from view in clearcase (by Claude TETE)
(defun clearcase-version-tree-graphical ()
  "Version tree of the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get full path of current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase version tree
        (async-shell-command (concat "clearvtree.exe \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; ClearCase explorer of file from view in clearcase (by Claude TETE)
(defun clearcase-explorer-graphical ()
  "ClearCase Explorer of the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get full path of current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when my-buffer
        ;; call clearcase explorer
        (async-shell-command (concat "clearexplorer.exe \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Version properties of file from view in clearcase (by Claude TETE)
(defun clearcase-version-properties-graphical ()
  "Version properties of the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get full path of current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase describe
        (async-shell-command (concat "cleardescribe.exe \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Properties of file from view in clearcase (by Claude TETE)
(defun clearcase-properties-graphical ()
  "Properties of the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get full path of current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase describe
        (async-shell-command (concat "cleardescribe.exe \"" (car my-buffer) "@@\""))
        )
      )
    )
  )

;; Checkin file from view in clearcase (by Claude TETE)
(defun clearcase-checkin-graphical ()
  "Checkin the current buffer if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get full path of current buffer
    (let ((my-buffer (clearcase-get-buffer-file-name)))
      ;; only when the buffer name is valid
      (when (and my-buffer (not (car (last my-buffer))))
        ;; call clearcase describe
        (async-shell-command (concat "cleardlg.exe /window 606f6 /windowmsg A065 /checkin \"" (car my-buffer) "\""))
        )
      )
    )
  )

;; Find checkout files from view in clearcase (by Claude TETE)
(defun clearcase-find-checkout-graphical ()
  "Find checkout for the current directory if it is from clearcase."
  (interactive)
  ;; do not show a new window
  (save-window-excursion
    ;; get full path of current buffer
    (let ((my-buffer (clearcase-get-buffer-directory-name)))
      ;; only when the buffer name is valid
      (when my-buffer
        ;; call clearcase describe
        (async-shell-command (concat "clearfindco.exe \"" my-buffer "\""))
        )
      )
    )
  )


(provide 'function-clearcase)

;;; function-clearcase.el ends here
