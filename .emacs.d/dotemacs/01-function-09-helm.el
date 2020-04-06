;;; 01-function-09-helm.el --- add some function about helm mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about Helm mode]
;; [SUBDEFAULT.t]
;;


;;; Code:

;;
;;;
;;;; HELM-FILES
;;; do not display dot paths in list of files with helm
(defvar helm-ff-directory-no-dots-p t)
;; rework original function `helm-ff-directory-files'
(defun helm-ff-directory-files-no-dots (helm-ff-directory-files directory &optional full)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but do not always returns the
dotted filename '.' and '..' even on root directories in Windows
systems unlike original function."
  ;; original code
  (setq directory (file-name-as-directory
                    (expand-file-name directory)))
  ;; original variables
  (let* (file-error
         (ls   (condition-case err
                   (helm-list-directory directory)
                 ;; Handle file-error from here for Windows
                 ;; because predicates like `file-readable-p' and friends
                 ;; seem broken on emacs for Windows systems (always returns t).
                 ;; This should never be called on GNU/Linux/Unix
                 ;; as the error is properly intercepted in
                 ;; `helm-find-files-get-candidates' by `file-readable-p'.
                 (file-error
                  (prog1
                      (list (format "%s:%s"
                                    (car err)
                                    (mapconcat 'identity (cdr err) " ")))
                    (setq file-error t)))))
          (dot  (concat directory "."))
          (dot2 (concat directory ".."))
          ;; new variable
          ret)
    (puthash directory (+ (length ls) 2) helm-ff--directory-files-hash)
    ;; original code
    ;(append (and (not file-error) (list dot dot2)) ls)))
    ;; new code
    (if helm-ff-directory-no-dots-p
      (setq ret ls)
      (if (not file-error)
        (setq ret (append (list dot dot2) ls))
        (setq ret ls)))
    ret))

;; replace helm-ff-directory-files by helm-ff-directory-files-no-dots by using advice
(advice-add 'helm-ff-directory-files :around #'helm-ff-directory-files-no-dots)
;; to remove advice on original function
;(advice-remove 'helm-ff-directory-files #'helm-ff-directory-files-no-dots)

;;
;;;
;;;; HELM BOOKMARK FIND FILES JUMP
(defun helm-bookmark-find-files-jump (candidate)
  "Jump to bookmark in find files from keyboard."
  (let ((current-prefix-arg helm-current-prefix-arg)
         non-essential
        (candidate-path (bookmark-get-filename (bookmark-get-bookmark candidate 'noerror))))
    (if (file-directory-p candidate-path)
      (helm-find-files-1 candidate-path)
      (message (concat "Not a directory: " candidate-path)))))


(provide '01-function-09-helm)

;;; 01-function-09-helm.el ends here
