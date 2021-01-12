;;; 01-function-09-helm.el --- add some function about helm mode -*- lexical-binding: t -*-

;; Copyright (c) 2017-2021 Claude Tete
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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: January 2021

;;; Commentary:
;;
;; [SUBHEADER.custom function about Helm mode]
;; [SUBDEFAULT.t]
;;


;;; Code:

;;
;;;
;;;; HELM-FILES
;; from Spacemacs no-dots (https://github.com/TheBB/spacemacs-layers/tree/master/layers/no-dots)
(defvar no-dots-whitelist
  '()
  "List of helm buffers in which to show dots.")

(require 'cl-lib)

(defun no-dots/whitelistedp ()
  (member (with-helm-buffer (buffer-name)) no-dots-whitelist))

(defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file &optional reverse skip-boring-check)
  (when (or (no-dots/whitelistedp)
            (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)))
    (funcall fcn file)))

(defun no-dots/helm-file-completion-source-p (&rest args) t)

(defun no-dots/helm-attrset (fcn attribute-name value &optional src)
  (let ((src (or src (helm-get-current-source))))
    (when src
      (funcall fcn attribute-name value src))))

(defun no-dots/helm-find-files-up-one-level (fcn &rest args)
  (advice-add 'helm-file-completion-source-p
              :around 'no-dots/helm-file-completion-source-p)
  (advice-add 'helm-attrset
              :around 'no-dots/helm-attrset)
  (let ((res (apply fcn args)))
    (advice-remove 'helm-file-completion-source-p
                   'no-dots/helm-file-completion-source-p)
    (advice-remove 'helm-attrset
                   'no-dots/helm-attrset)
    res))

(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :around 'no-dots/helm-ff-filter-candidate-one-by-one)
  (advice-add 'helm-find-files-up-one-level
              :around 'no-dots/helm-find-files-up-one-level))

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
