;;; 01-function-05-file.el --- add some function about file management -*- lexical-binding: t -*-

;; Copyright (c) 2017-2019 Claude Tete
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
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about file management]
;; [SUBDEFAULT.t]
;;


;;; Code:

;;; rename the current opened file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
      (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
            name (file-name-nondirectory new-name)))))))

;;; delete the current file
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
         (buffer (current-buffer))
         (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
      (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; steal from http://stackoverflow.com/a/208787
(defun set-buffer-file-writable ()
  "Make the file shown in the current buffer writable.
Make the buffer writable as well."
  (interactive)
  (read-only-mode 'toggle)
  (unix-output "chmod" "+w" (buffer-file-name))
  (message (trim-right '(?\n) (unix-output "ls" "-l" (buffer-file-name)))))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; remove read-only attribute to a buffer/file
    (global-set-key     (kbd "C-x C-q")         'set-buffer-file-writable)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;; use by set-buffer-file-writable function
;; steal from http://stackoverflow.com/a/208787
(defun unix-output (command &rest args)
  "Run a unix command and, if it returns 0, return the output as a string.
Otherwise, signal an error.  The error message is the first line of the output."
  (let ((output-buffer (generate-new-buffer "*stdout*")))
    (unwind-protect
      (let ((return-value (apply 'call-process command nil
                            output-buffer nil args)))
        (set-buffer output-buffer)
        (save-excursion
          (unless (= return-value 0)
            (goto-char (point-min))
            (end-of-line)
            (if (= (point-min) (point))
              (error "Command failed: %s%s" command
                (with-output-to-string
                  (dolist (arg args)
                    (princ " ")
                    (princ arg))))
              (error "%s" (buffer-substring-no-properties (point-min)
                            (point)))))
          (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output-buffer))))

;; use by unix-output function
;; steal from http://stackoverflow.com/a/208787
(defun trim-right (bag string &optional start end)
  (setq bag (if (eq bag t) '(?\  ?\n ?\t ?\v ?\r ?\f) bag)
    start (or start 0)
    end (or end (length string)))
  (while (and (> end 0)
           (member (aref string (1- end)) bag))
    (decf end))
  (substring string start end))


(provide '01-function-05-file)

;;; 01-function-05-file.el ends here
