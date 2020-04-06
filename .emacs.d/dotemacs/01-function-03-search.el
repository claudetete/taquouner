;;; 01-function-03-search.el --- add some function about searching in buffer

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
;; [SUBHEADER.custom function about searching in buffer text]
;; [SUBDEFAULT.t]
;;


;;; Code:

;; search the word at the point for the whole buffer (by Claude TETE)
(defun occur-word-at-point ()
  "Search the word under cursor in the current buffer."
  (interactive)
  (let (word prompt input)
    (setq word (tqnr-select-word))
    (if word
      (setq prompt (concat "List lines matching regexp (default " word "): "))
      (setq prompt "List lines matching regexp: "))
    (setq input (completing-read prompt 'try-completion nil nil nil nil))
    (if (not (equal "" input))
      (setq word input))
    (occur word)
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; search all occurences at point
    (global-set-key     (kbd "C-M-c")   'occur-word-at-point)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;;
;; occur when incremental search (by Fabrice Niessen)
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(with-eval-after-load "isearch"
  ;; occur when incremental search
  (define-key isearch-mode-map (kbd "C-o")        'isearch-occur))


;;
;; incremental search the word at the point (from www.emacswiki.org)
;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  "Set initialization of isearch."
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update)
  )

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point (optional REGEXP-P and NO-RECURSIVE-EDIT)."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
            (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
        (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)
        )
      )
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; search the word at point (cannot bind C-M-x still run 'eval-defun)
    (global-set-key     (kbd "C-M-v")   'isearch-forward-at-point)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;; ---------------- matching word pairs ------------------
;; The idea here is that while emacs has built-in support for matching
;; things like parentheses, I work with a variety of syntaxes that use
;; balanced keyword pairs, such as "begin" and "end", or "#if" and
;; "#endif".  So this mechanism searches for the balanced element
;; of such ad-hoc constructions. (by Scott McPeak)
;;
;; TODO: Currently, there is no support for skipping things that are
;; in string literals, comments, etc.  I think that would be possible
;; just by having appropriate regexs for them and skipping them when
;; they occur, but I haven't tried yet.
(defun find-matching-element (search-func offset open-regex close-regex)
  "Search forwards or backwards (depending on `search-func') to find
   the matching pair identified by `open-regex' and `close-regex'."
  (let ((nesting 1)                ; number of pairs we are inside
        (orig-point (point))       ; original cursor loc
        (orig-case-fold-search case-fold-search))
    (setq case-fold-search nil)        ; case-sensitive search
    (goto-char (+ (point) offset))     ; skip the `open-regex' at cursor
    (while (and (> nesting 0)
                (funcall search-func
                  (concat "\\(" open-regex "\\)\\|\\(" close-regex "\\)") nil t))
      (if (string-match open-regex (match-string 0))
        (setq nesting (+ nesting 1))
        (setq nesting (- nesting 1))
      ))
    (setq case-fold-search orig-case-fold-search)
    (if (eq nesting 0)
      ; found the matching word, move cursor to the beginning of the match
      (goto-char (match-beginning 0))
      ; did not find the matching word, report the nesting depth at EOF
      (progn
        (goto-char orig-point)
        (error (format "Did not find match; nesting at file end is %d" nesting))
      )
      )))

;; find the matching word/character /* it's a pain to point the word begining */
;; This is what I bind to C-left and C-right with some mode. (inspired by Scott McPeak)
(defun find-matching-keyword ()
  "Find the matching keyword of a balanced pair."
  (interactive)
  (cond
    ;; these first two come from lisp/emulation/vi.el
    ((looking-at "[[({]") (forward-sexp 1))
    ((looking-back "[])}]") (backward-sexp 1))
    ;;
    ;; rtp file from RTRT
    ((looking-at "<unit_testing>")
      (when (eq major-mode 'nxml-mode)
        (find-matching-element 're-search-forward 14 "<unit_testing>" "</unit_testing>")))
    ((looking-at "</unit_testing>")
      (when (eq major-mode 'nxml-mode)
        (find-matching-element 're-search-backward 0 "</unit_testing>" "<unit_testing>")))
    ;;
    ;; RTRT script .ptu
    ;; "\\b": word boundary assertion, needed because one delimiter is
    ;; a substring of the other
    ;; ELEMENT
    ((looking-at "SERVICE")
      (when (eq major-mode 'rtrt-script-mode)
        (find-matching-element 're-search-forward 7 "\\bSERVICE\\b" "END SERVICE")))
    ((looking-at "END SERVICE")
      (when (eq major-mode 'rtrt-script-mode)
        (find-matching-element 're-search-backward 0 "END SERVICE" "\\bSERVICE\\b")))
    ;; TEST
    ((looking-at "TEST")
      (when (eq major-mode 'rtrt-script-mode)
        (find-matching-element 're-search-forward 5 "\\bTEST\\b" "END TEST")))
    ((looking-at "END TEST")
      (when (eq major-mode 'rtrt-script-mode)
        (find-matching-element 're-search-backward 0 "END TEST" "\\bTEST\\b")))
    ;; ELEMENT
    ((looking-at "ELEMENT")
      (when (eq major-mode 'rtrt-script-mode)
        (find-matching-element 're-search-forward 7 "\\bELEMENT\\b" "END ELEMENT")))
    ((looking-at "END ELEMENT")
      (when (eq major-mode 'rtrt-script-mode)
        (find-matching-element 're-search-backward 0 "END ELEMENT" "\\bELEMENT\\b")))
    ;;
    ;; C/C++
    ((looking-at "#if")
      (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (find-matching-element 're-search-forward 3 "#if" "#endif")))
    ((looking-at "#endif")
      (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (find-matching-element 're-search-backward 0 "#endif" "#if")))
    ;;
    ;; FitNesse
    ((looking-at "!\\*")
      (when (eq major-mode 'fitnesse-mode)
        (find-matching-element 're-search-forward 2 "!\\*" "\\*!")))
    ((looking-at "\\*!")
      (when (eq major-mode 'fitnesse-mode)
        (find-matching-element 're-search-backward 0 "\\*!" "!\\*")))
    ;;
    ;;(t (error "Cursor is not on ASSERT nor RETRACT"))
    (t t))
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; navigate to start/end of balance expressions
    (global-set-key     (kbd "M-]")     'find-matching-keyword)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;;
;; search a tab in buffer (by Claude TETE)
(defun search-tab ()
  "Search a tab in the current buffer."
  (interactive)
  (occur "[\t\v]")
  )

;; search a fault size in buffer (by Claude TETE)
(when (string= tqnr-profile-name "alstom-transport")
  (defun search-fault-size ()
    "Search a sizing fault in the current buffer."
    (interactive)
    ;; line more than 80 column
    (occur ".\\{81,\\}")
    )
  ) ; (when (string= tqnr-profile-name "alstom-transport")


(provide '01-function-03-search)

;;; 01-function-03-search.el ends here
