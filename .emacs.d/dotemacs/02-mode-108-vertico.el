;;; 02-mode-108-vertico.el --- configuration of vertico -*- lexical-binding: t -*-

;; Copyright (c) 2023 Claude Tete
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
;; Created: March 2023
;; Last-Updated: March 2023

;;; Commentary:
;;
;; [SUBHEADER.vertico: vertical list for completions]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package vertico
  :ensure t

  :init
  (vertico-mode)
  (vertico-multiform-mode)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow only (never shrink) the Vertico minibuffer
  (setq vertico-resize 'grow-only)

  ;; Configure the default sorting function for symbols and files
  ;; See `vertico-sort-function'.
  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file grid)
          (file (vertico-sort-function . vertico-sort-directories-first))))

;; Forcibly override the sorting function for `consult-line'.
  ;; See `vertico-sort-override-function'.
  (setq vertico-multiform-commands
        '((consult-line (vertico-sort-override-function . vertico-sort-alpha))))

  (defun vertico-sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun vertico-sort-directories-first-alpha (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; ;; Change the default sorting function for files (by example in find-file)
  ;; ;; sort only alphabetically and put folders first
  ;; (setq vertico-multiform-categories
  ;;       '((file grid)
  ;;         (file (vertico-sort-function . sort-directories-first))))

  ;; ;; Sort directories before files
  ;; (defvar vertico-sort-function-for-file #'vertico-sort-alpha)
  ;; (defun sort-directories-first (files)
  ;;   (setq files (vertico-sort-function-for-file files))
  ;;   (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
  ;;          (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; (defun vertico-sort-function-for-file-toggle (event)
  ;;   (interactive "P")
  ;;   (if (= vertico-sort-function-for-file #'vertico-sort-alpha)
  ;;     (setq vertico-sort-function-for-file #'vertico-sort-history-alpha)
  ;;     (setq vertico-sort-function-for-file #'vertico-sort-alpha)
  ;;     ))

  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    "Make vertico-insert add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  (defun vertico-toggle-sort-directories-first ()
    (interactive)
    ;; toggle between default and directory first sorting
    (if (eq vertico-sort-function 'vertico-sort-directories-first)
        (set (make-local-variable 'vertico-sort-function) 'vertico-sort-history-length-alpha)
      (if (eq vertico-sort-function 'vertico-sort-history-length-alpha)
          (set (make-local-variable 'vertico-sort-function) 'vertico-sort-directories-first)
        ;; toggle between default and directory first sorting when only alpha sorting is enabled
        (if (eq vertico-sort-function 'vertico-sort-directories-first-alpha)
          (set (make-local-variable 'vertico-sort-function) 'vertico-sort-alpha)
          (set (make-local-variable 'vertico-sort-function) 'vertico-sort-directories-first-alpha))))
    (setq vertico--input t)
    (vertico-buffer-file-name-reselect)
    (vertico--update))

  (defun vertico-toggle-sort-alpha ()
    (interactive)
    ;; toggle between default and only alpha sorting when directories first option is enabled
    (if (eq vertico-sort-function 'vertico-sort-directories-first)
        (set (make-local-variable 'vertico-sort-function) 'vertico-sort-directories-first-alpha)
      (if (eq vertico-sort-function 'vertico-sort-directories-first-alpha)
          (set (make-local-variable 'vertico-sort-function) 'vertico-sort-directories-first)
        ;; toggle between default and only alpha sorting
        (if (eq vertico-sort-function 'vertico-sort-history-length-alpha)
            (set (make-local-variable 'vertico-sort-function) 'vertico-sort-alpha)
            (set (make-local-variable 'vertico-sort-function) 'vertico-sort-history-length-alpha))))
    (setq vertico--input t)
    (vertico-buffer-file-name-reselect)
    (vertico--update))

  (defun vertico-buffer-file-name-reselect ()
    (when (and vertico-buffer-file-name-backup
               (not (string-equal vertico-buffer-file-name vertico-buffer-file-name-backup))
               (string-equal (file-name-nondirectory vertico-buffer-file-name-backup) (file-name-nondirectory (vertico--candidate))))
      (setq vertico-buffer-file-name vertico-buffer-file-name-backup)))

  ;; make sure previous directory is auto selected when goes to parent folder, from (defvar vertico-previous-directory to (define-advice vertico--update
  (defvar vertico-previous-directory nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

  (defun vertico-set-previous-directory (n)
    "Set the directory that was just exited from within find-file."
    (when (> (point) (minibuffer-prompt-end))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (minibuffer-prompt-end) t)
          ;; set parent directory
          (setq vertico-previous-directory (buffer-substring (1+ (point)) (point-max)))
          ;; set back to nil if not sorting by directories or what was deleted is not a directory
          (when (not (string-suffix-p "/" vertico-previous-directory))
            (setq vertico-previous-directory nil))
          t))))

  (advice-add #'vertico-directory-up :before #'vertico-set-previous-directory)

  (defvar vertico-buffer-file-name nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

  (defvar vertico-buffer-file-name-backup nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")

  (defun vertico-set-buffer-file-name (prompt mustmatch)
    (let ((filename (buffer-file-name)))
      (if (and filename (file-exists-p filename))
          (progn
            (setq vertico-buffer-file-name filename)
            (setq vertico-buffer-file-name-backup filename))
        (progn
          (setq vertico-buffer-file-name nil)
          (setq vertico-buffer-file-name-backup nil)))))

  (advice-add #'find-file-read-args :before #'vertico-set-buffer-file-name)

  (define-advice vertico--update (:after (&rest _) choose-candidate)
    "Pick the previous directory rather than the prompt after updating candidates."
    (when vertico-previous-directory ; select previous directory
      (setq vertico--index (or (seq-position vertico--candidates vertico-previous-directory)
                               vertico--index))
      (setq vertico-previous-directory nil))
    (when vertico-buffer-file-name
      (setq vertico--index (or (seq-position vertico--candidates (file-name-nondirectory vertico-buffer-file-name))
                               vertico--index)))
    (setq vertico-buffer-file-name nil))

  :config
  (when tqnr-section-mode-consult
    ;; disable group for the usage of consult-ripgrep (TODO disable only for consult-ripgrep)
    (setq vertico-group-format nil))

  :bind
  (:map vertico-map
        ("<escape>" . minibuffer-keyboard-quit)
        ;; reproduce helm behavior
        ("<left>"   . vertico-directory-up)
        ("<right>"  . vertico-insert)
        ("M-D"      . vertico-toggle-sort-directories-first)
        ("M-A"      . vertico-toggle-sort-alpha)
        ("<prior>"  . vertico-scroll-down)
        ("<next>"   . vertico-scroll-up)
        )
  (:map vertico-multiform-map
        ("M-D"       . vertico-toggle-sort-directories-first)
        ("M-A"       . vertico-toggle-sort-alpha)
        ("<M-left>"  . vertico-grid-left)
        ("<M-right>" . vertico-grid-right)
        )
  )

(use-package vertico-directory
  :after vertico
  :ensure nil

  :bind (:map vertico-map
              ("<left>"  . vertico-directory-up)
              ("<right>" . vertico-insert)
              ("RET"     . vertico-directory-enter)
              ("M-RET"   . vertico-exit)
              ("M-S-RET" . vertico-exit-input)
              ("DEL"     . vertico-directory-delete-char)
              ("M-DEL"   . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(provide '02-mode-108-vertico)

;;; 02-mode-108-vertico.el ends here
