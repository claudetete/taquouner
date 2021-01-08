;;; fitnesse-mode.el --- a mode to edit FitNesse MarkUp files

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.2
;; Created: November 2017
;; Last-Updated: October 2020

;;; Commentary:
;;
;; TODO motion between collapsable sections
;; TODO fold collapsable sections (outline integration), workaround with custom function
;; TODO follow links (cross reference)
;; TODO snippet to insert anything
;; TODO smartparens integration


;;; Change Log:
;; 2020-10-15 (0.2)
;     add call of external formater
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

(defcustom fitnesse-pretty-print-executable nil
  "FitNesse pretty print tool path."
  :type 'string
  :group 'fitnesse)

(defcustom fitnesse-server-url nil
  "FitNesse server url."
  :type 'string
  :group 'fitnesse)

(defcustom fitnesse-server-port nil
  "FitNesse server port."
  :type 'string
  :group 'fitnesse)

(defcustom fitnesse-translate-executable nil
  "FitNesse translate name tool path."
  :type 'string
  :group 'fitnesse)

(defcustom fitnesse-translate-flags nil
  "FitNesse translate name tool flags."
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
    (define-key fitnesse-mode-map (kbd "<M-up>")   #'fitnesse-up-section)
    (define-key fitnesse-mode-map (kbd "<M-down>") #'fitnesse-down-section)
    (define-key fitnesse-mode-map (kbd "C-c a a")  #'fitnesse-align)
    (define-key fitnesse-mode-map (kbd "<f11>")    #'fitnesse-insert-debug)
    (define-key fitnesse-mode-map (kbd "<S-f11>")  #'fitnesse-insert-get-leg-data)
    ))

;; outline support
(defvar fitnesse-mode-outline-regexp "\\!\\*+")

(defconst fitnesse-mode-font-lock-keyword
  (list
    ;; comment
    ;; '("^\\s-*\\(#.*\\)$"
    ;;    (1 font-lock-comment-face nil t))
    ;; code multiline
    '("^\\s-*\\({{{\\(.\\|\n\\)*?}}}\\)"
       (1 font-lock-comment-face nil t))
    ;; collapsable section start
    '("^\\s-*\\(!\\*+[><]?\\)\\s-*\\(.*\\)\\s-*$"
       (1 font-lock-keyword-face nil t) (2 font-lock-string-face nil t))
    ;; collapsable section end
    '("^\\s-*\\(\\*+!\\)"
       (1 font-lock-keyword-face nil t))
    ;; collapsable section end
    '("^\\s-*\\(\\*+!\\)"
       (1 font-lock-keyword-face nil t))
    ;; headers
    '("^\\s-*\\(![1-9]\\)\\(.*\\)$"
       (1 font-lock-keyword-face nil t) (2 font-lock-string-face nil t))
    ;; image
    '("^\\s-*\\(!img\\(?:-[rl]\\)?\\)"
       (1 font-lock-keyword-face nil t))
    ;; note
    '("^\\s-*\\(!note\\)\\(.*\\)$"
       (1 font-lock-keyword-face nil t) (2 font-lock-comment-face nil t))
    ;; function
    '("^\\s-*!|\\s-*\\([A-Za-z0-9_]*\\)\\s-*|"
       (1 font-lock-function-name-face nil t))
    ;; define
    '("^\\s-*\\(!define\\)"
       (1 font-lock-keyword-face nil t))
    ;; style_code
    '("\\(!style_\\(?:code\\|note\\)\\)\\[\\(.*?\\)\\]"
       (1 font-lock-variable-name-face nil t) (2 font-lock-comment-face nil t))
    ;; bold and italics
    '("\\('''?.*?'?''\\)"
       (1 font-lock-type-face nil t))
    ;; strike
    '("\\(--.*?--\\)"
       (1 font-lock-type-face nil t))
    ;; variable
    '("\\(\\$\\(?:{[A-Za-z0-9_]*?\\b}\\|[A-Za-z0-9_]*\\b\\)\\)"
       (1 font-lock-variable-name-face nil t))
    ;; converter
    '("\\(\\b[A-Za-z0-9]*:\\)"
       (1 font-lock-keyword-face nil t))
    ;; link
    '("[^\\.<A-Za-z0-9]\\(\\(?:[\\.<][A-Z][a-zA-Z0-9]*\\)\\(?:\\.[A-Z][a-zA-Z0-9]*\\)*\\(#[0-9]\\)*\\)"
       (1 font-lock-preprocessor-face nil t))
    ;; text
    '("^\\s-*\\([^!|\\*].*\\)$"
       (1 font-lock-string-face nil t))
    ;; constant
    '("\\b\\([NEWS]?[0-9\\.]\\{1,\\}\\)\\b"
       (1 font-lock-constant-face nil t))
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
  ;(set-buffer-file-coding-system "utf-8")
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

(defun fitnesse-insert-debug ()
  "Insert a debug point.
!| FitnesseDebug       |
| breakPoint | etapeId |
| true       |  |
"
  (interactive)
  (insert "\n!| FitnesseDebug |\n| breakPoint | etapeId |\n| true       |  |\n\n")
  (backward-char 4))

(defun fitnesse-insert-get-leg-data ()
  "Insert get debug info for fpln leg.
!|Get_LegsData|ACTIVE                                                                                                                                                                                                                                          |
|legNumber    |legType?|legIdent?|transitionType?|transitionSubtype?|legTerminaisonPosition?|legTerminaisonPositionLatitude?|legTerminaisonPositionLongitude?|turnDirectionConstraint?|trueCourseOrTrueHeading?|courseHdgConstraintType?|discontinuityPresence?|
|1            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |
|2            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |
|3            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |
|4            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |
|5            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |
|6            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |
"
  (interactive)
  (insert "\n!|Get_LegsData|ACTIVE                                                                                                                                                                                                                                          |\n|legNumber    |legType?|legIdent?|transitionType?|transitionSubtype?|legTerminaisonPosition?|legTerminaisonPositionLatitude?|legTerminaisonPositionLongitude?|turnDirectionConstraint?|trueCourseOrTrueHeading?|courseHdgConstraintType?|discontinuityPresence?|\n|1            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |\n|2            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |\n|3            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |\n|4            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |\n|5            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |\n|6            |        |         |               |                  |                       |                               |                                |                        |                        |                        |                      |\n\n")
  )

(defun fitnesse-align ()
  "Call external tool to pretty print region or buffer."
  (interactive)
  (if (use-region-p)
    (progn
      (message "align region with %s..." (eval fitnesse-pretty-print-executable))
      (shell-command-on-region (region-beginning) (region-end)
        (eval fitnesse-pretty-print-executable) t t t))
    (progn
      (shell-command
        (format "%s --file %s"
          (shell-quote-argument (eval fitnesse-pretty-print-executable))
          (shell-quote-argument (buffer-file-name))))
      (revert-buffer t t t))))

(defun fitnesse-get-fit-name-from-path ()
  (shell-command-to-string (format "%s %s %s"
                             (shell-quote-argument (eval fitnesse-translate-executable))
                             (shell-quote-argument (eval fitnesse-translate-flags))
                             (shell-quote-argument (buffer-file-name)))))

(defun fitnesse-browse ()
  "Open current test in configured browser."
  (interactive)
  (let ((fit-name (fitnesse-get-fit-name-from-path)))
    (browse-url-generic (concat fitnesse-server-url "/" fit-name))
    )
  )


(provide 'fitnesse-mode)

;;; fitnesse-mode.el ends here
