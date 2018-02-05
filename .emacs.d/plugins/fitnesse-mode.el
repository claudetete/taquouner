;;; fitnesse-mode.el --- a mode to edit FitNesse MarkUp files

;; Copyright (c) 2017 Claude Tete
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

;; Keywords: fitnesse, markup, fit, wiki
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.1
;; Created: November 2017
;; Last-Updated: November 2017

;;; Commentary:
;;
;; TODO syntax highlighting
;; TODO motion between collapsable sections
;; TODO fold collapsable sections (outline integration)
;; TODO follow links (cross reference)
;; TODO snippet to insert anything
;; TODO smartparens integration


;;; Change Log:
;; 2017-11-29 (0.1)
;;    from scratch


;;; Code:

;;
;;;
;;;; CUSTOM
(defgroup fitnesse nil
  "FitNesse mode to edit wiki style."
  :group 'languages
  :prefix "fitnesse")

(defcustom fitnesse-root nil
  "FitNesse root path."
  :type 'string
  :group 'fitnesse)

;;
;;;
;;;; VARIABLES
(defvar fitnesse-mode-hook nil
  "Run after loading fitnesse-mode.")

(defvar fitnesse-mode-map nil
  "Bind map for fitnesse-mode.")
(if fitnesse-mode-map
  nil
  (progn
    (setq fitnesse-mode-map (make-sparse-keymap))
    (define-key fitnesse-mode-map (kbd "<M-up>") 'fitnesse-up-section)
    (define-key fitnesse-mode-map (kbd "<M-down>") 'fitnesse-down-section)
    ))

;; outline support
(defvar fitnesse-mode-outline-regexp "\\!\\*+")

(defconst fitnesse-mode-font-lock-keyword
  (list
    ;; comment
    '("^\\s-*\\(#.*\\)$"
       (1 font-lock-comment-face nil t))
    ;; code multiline
    '("^\\s-*\\({{{\\(.\\|\n\\)*?}}}\\)"
       (1 font-lock-comment-face nil t))
    ;; collapsable section start
    '("^\\s-*\\(!\\*+[><]?\\)\\s-*\\(.*\\)\\s-*$"
       (1 font-lock-keyword-face nil t) (2 font-lock-function-name-face nil t))
    ;; collapsable section end
    '("^\\s-*\\(\\*+!\\)"
       (1 font-lock-keyword-face nil t))
    ;; collapsable section end
    '("^\\s-*\\(\\*+!\\)"
       (1 font-lock-keyword-face nil t))
    ;; headers
    '("^\\s-*\\(![1-9]\\)\\(.*\\)$"
       (1 font-lock-keyword-face nil t) (2 font-lock-function-name-face nil t))
    ;; image
    '("^\\s-*\\(!img\\(?:-[rl]\\)?\\)"
       (1 font-lock-keyword-face nil t))
    ;; note
    '("^\\s-*\\(!note\\)\\(.*\\)$"
       (1 font-lock-keyword-face nil t) (2 font-lock-comment-face nil t))
    ;; style_code
    '("\\(!style_\\(?:code\\|note\\)\\)\\[\\(.*?\\)\\]"
       (1 font-lock-variable-name-face nil t) (2 font-lock-comment-face nil t))
    ;; bold and italics
    '("\\('''?.*?'?''\\)"
       (1 font-lock-type-face nil t))
    ;; strike
    '("\\(--.*?--\\)"
       (1 font-lock-type-face nil t))
    ;; link
    '("[^\\.A-Za-z0-9]\\(\\(?:\\.[A-Z][a-zA-Z0-9]*\\)\\(?:\\.[A-Z][a-zA-Z0-9]*\\)*\\)"
       (1 font-lock-preprocessor-face nil t))
    )
  "Keyword for fitnesse-mode.")

(defvar fitnesse-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; ''' character is punctuation
    (modify-syntax-entry ?\' "." syntax-table)
    ;; '#' character is start of comment
    (modify-syntax-entry ?\# "<" syntax-table)
    ;; '\n' character is end of comment
    (modify-syntax-entry ?\n ">" syntax-table)
    ;; '!' character is punctuation
    (modify-syntax-entry ?! "." syntax-table)
    ;; ''' character is punctuation
    (modify-syntax-entry ?\' "." syntax-table)
    ;; '\' character is punctuation (no more escape)
    (modify-syntax-entry ?\\ "." syntax-table)
    syntax-table)
  "Syntax table for fitnesse-mode.")

(defconst fitnesse-mode-version "0.1"
  "The fitnesse-mode version string.")


;;
;;;
;;;; FUNCTION (interactive)

;;;###autoload
(defun fitnesse-mode ()
  "Major mode for editing FitNesse wiki files."
  (interactive)
  ;; empty environment
  (kill-all-local-variables)
  ;; set major mode
  (setq major-mode 'fitnesse-mode)
  ;; set name mode
  (setq mode-name "FitNesse")
  ;; set comment character
  ;;(set (make-local-variable 'comment-start) "# ")
  ;;(set (make-local-variable 'comment-end)   "")
  ;; set key binding
  (use-local-map fitnesse-mode-map)
  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(fitnesse-mode-font-lock-keyword nil t nil))
  ;; set syntax table (character meanings)
  (set-syntax-table fitnesse-mode-syntax-table)
  ;; outline
  (set (make-local-variable 'outline-regexp) 'fitnesse-mode-outline-regexp)
  ;; show image using builtin iimage mode
  (iimage-mode t)
  ;; run history mode hook
  (run-hooks 'text-mode-hook 'fitnesse-mode-hook))

(defun fitnesse-goto-link ()
  "Open link at point"
  (interactive (fitnesse-thing-at-point))
  (let ((link-path fitnesse-get-link-realpath))
    (if (and link-path (file-exists-p link-path))
      (fitnesse-open-path)
      (message "Link not found."))))


(put 'fitnesse 'end-op (lambda () (skip-chars-backward ".A-Za-z0-9")))
(put 'fitnesse 'beginning-op (lambda () (skip-chars-forward ".A-Za-z0-9")))

(defun fitnesse-thing-at-point ()
  (let ()
    (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
      (thing-at-point 'fitnesse))))

;;
;; MOVE
;; move to the previous up heading
(defun fitnesse-up-section ()
  "Go up in script SERVICE"
  (interactive)
  (re-search-backward "^\\s-*\\(!\\*+[><]?\\)\\s-*\\(.*\\)\\s-*$"))

;; move to the previous up heading
(defun fitnesse-down-section ()
  "Go down in script SERVICE"
  (interactive)
  (re-search-forward "^\\s-*\\(\\*+!\\)"))



;; associate context.txt files to fitnesse-mode
(add-to-list 'auto-mode-alist '("content\\.txt\\'" . fitnesse-mode))


(provide 'fitnesse-mode)

;;; fitnesse-mode.el ends here
