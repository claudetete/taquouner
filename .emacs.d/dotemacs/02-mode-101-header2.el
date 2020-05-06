;;; 02-mode-101-header2.el --- configuration of header2 mode -*- lexical-binding: t -*-

;; Copyright (c) 2020 Claude Tete
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
;; Created: April 2020
;; Last-Updated: May 2020

;;; Commentary:
;;
;; [SUBHEADER.Create/Update header of files]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package header2
  :bind
  ("C-x M-s" . update-file-header)

  :config
  (setq header-date-format "%B %Y")
  (setq header-copyright-notice "Copyright (c) \n")

  ;; from header2.el to add lexical-binding
  (defsubst header-title2 ()
    "Insert buffer's file name and leave room for a description."
    (insert (concat comment-start
              (and (= 1 (length comment-start))  header-prefix-string)
              (if (buffer-file-name)
                (file-name-nondirectory (buffer-file-name))
                (buffer-name))
              " ---  " "-*- lexical-binding: t -*-\n"))
    (setq return-to  (1- (point))))

  (defsubst header-empty-line ()
    "Insert an empty line (not a comment)"
    (insert "\n"))

  (defsubst header-next-page ()
    "Insert an empty line (not a comment)"
    (insert "\n"))

  (defsubst header-not-part-of-emacs ()
    "Insert an empty line (not a comment)"
    (insert (concat header-prefix-string "This file is NOT part of GNU Emacs.\n")))

  (defun header-copyright2 ()
    "Insert `header-copyright-notice', unless nil."
    (when header-copyright-notice
      (insert (concat header-prefix-string header-copyright-notice))))

  ;; from header2.el to remove section start
  (defsubst header-commentary2 ()
    "Insert \"Commentary: \" line."
    (insert (concat comment-start header-prefix-string "Commentary: \n")))

  ;; from header2.el to remove section start
  (defsubst header-code2 ()
    "Insert \"Code: \" line."
    (insert (concat comment-start header-prefix-string "Code:" (nonempty-comment-end) "\n\n\n")))

  (setq make-header-hook '(
                            header-title2               ; ;;; filename.el --- Short description -*- lexical-binding: t -*-
                            header-empty-line           ;
                            header-copyright2           ; ;; Copyright (c) YEAR User Name
                            header-blank                ; ;;
                            header-not-part-of-emacs    ; ;; This file is NOT part of GNU Emacs.
                            header-free-software        ; ;; GPL summary
                            header-next-page            ; 
                            header-author               ; ;; Author: (`user-full-name')
                            header-version              ; ;; Version: X.X
                            header-creation-date        ; ;; Created: MONTH YEAR
                            header-modification-date    ; ;; Last-Updated: April 2020
                            header-next-page            ; 
                            header-commentary2          ; ;; Commentary:
                            header-empty-line           ;
                            header-next-page            ; 
                            header-code2                ; ;; Code:
                            ))

  ;; update copyright when update header
  (defun header-update-copyright ()
    (let (year-begin
           (year-end (format-time-string "%Y")))
      ;; check year already insert
      (when (looking-at "[[:digit:]]+")
        (setq year-begin (buffer-substring (match-beginning 0) (match-end 0)))
        (when (string-equal year-begin year-end)
          (setq year-begin nil)))
      ;; remove any previously insert year and name
      (delete-and-forget-line)
      ;; insert year or years
      (if year-begin
        (insert (concat year-begin "-" year-end))
        (insert year-end))
      ;; insert name
      (insert (concat " " user-login-name))))

  ;; update version (from https://emacs.stackexchange.com/questions/13599/header2-el-how-to-increment-the-version-number-with-dot)
  (defun header-update-version ()
    (let* ((v-str (delete-and-forget-line))
            ;; `version-to-list' will also check if the version string is valid
            (v-list (version-to-list v-str))
            (v-major (nth 0 v-list))
            (v-minor (nth 1 v-list)))
      (insert (format "%0d.%0d" v-major (1+ v-minor)))))

  (setq file-header-update-alist ())
  (register-file-header-action "Copyright (c) " #'header-update-copyright)
  (register-file-header-action "Version[ \t]*: " #'header-update-version)
  (register-file-header-action "Last-Updated[ \t]*: " 'update-last-modified-date)

  ;; override header2.el to remove call of end-of-line
  (defun header-multiline ()
    "Insert multiline comment.  The comment text is in `header-multiline'."
    (when header-multiline
      (let ((lineno  1)
             beg end nb-lines)
        (beginning-of-line)
        (if (nonempty-comment-end)
          (insert (concat "\n" comment-start))
          (header-blank)
          (insert (concat header-prefix-string)))
        (setq beg  (point))
        (insert header-multiline)
        (setq end       (point-marker)
          nb-lines  (count-lines beg end))
        (goto-char beg)
        (forward-line 1)
        (while (< lineno nb-lines)
          (insert (concat header-prefix-string))
          (forward-line 1)
          (setq lineno  (1+ lineno)))
        (goto-char end)
        (when (nonempty-comment-end) (insert "\n"))
        (insert (concat comment-end))
        (insert "\n")
        (unless (nonempty-comment-end)
          (header-blank)))))
  ) ;; (use-package header2


(provide '02-mode-101-header2)

;;; 02-mode-101-header2.el ends here
