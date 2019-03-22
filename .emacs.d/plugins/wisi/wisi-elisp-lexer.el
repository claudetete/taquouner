;;; wisi-elisp-lexer.el --- A lexer for wisi, implemented in elisp -*- lexical-binding:t -*-
;;
;; Copyright (C) 2017, 2018  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;;; History: see NEWS-wisi.text

(require 'cl-lib)
(require 'semantic/lex)
(require 'wisi-parse-common)

(cl-defstruct wisi-elisp-lexer
  id-alist ;; alist mapping strings to token ids; used by repair error
  keyword-table ;; obarray holding keyword tokens
  punctuation-table ;; obarray holding punctuation tokens
  punctuation-table-max-length ;; max string length in punctuation-table
  string-double-term ;; non-nil if strings delimited by double quotes
  string-quote-escape-doubled ;; Non-nil if a string delimiter is escaped by doubling it
  string-quote-escape
  ;; Cons (delim . character) where `character' escapes quotes in strings delimited by `delim'.
  string-single-term ;; non-nil if strings delimited by single quotes
  symbol-term ;; symbol for a terminal symbol token
  number-term ;; symbol for a terminal number literal token
  number-p ;; function that determines if argument is a number literal
  line-begin ;; vector of beginning-of-line positions in buffer
  last-line ;; index into line-begin of line containing last lexed token
  )

(defun wisi-elisp-lexer-reset (line-count lexer)
  "Reset lexer to start a new parse. LINE-COUNT is the count of lines in the current buffer."
  (setf (wisi-elisp-lexer-line-begin lexer) (wisi--set-line-begin line-count))
  (setf (wisi-elisp-lexer-last-line lexer) nil))

(defvar-local wisi--lexer nil
  "A `wisi-elisp-lexer' struct defining the lexer for the current buffer.")

(defun wisi-elisp-lexer--safe-intern (name obtable)
  (let ((var (intern-soft name obtable)))
    (and (boundp var) (symbol-value var))))

(cl-defun wisi-make-elisp-lexer (&key token-table-raw keyword-table-raw string-quote-escape-doubled string-quote-escape)
  "Return a ‘wisi-elisp-lexer’ object."
  (let* ((token-table (semantic-lex-make-type-table token-table-raw))
	 (keyword-table (semantic-lex-make-keyword-table keyword-table-raw))
	 (left-paren (cadr (wisi-elisp-lexer--safe-intern "left-paren" token-table)))
	 (right-paren (cadr (wisi-elisp-lexer--safe-intern "right-paren" token-table)))
	 (punctuation-table (wisi-elisp-lexer--safe-intern "punctuation" token-table))
	 (punct-max-length 0)
	 (number (cadr (wisi-elisp-lexer--safe-intern "number" token-table)))
	 (symbol (cadr (wisi-elisp-lexer--safe-intern "symbol" token-table)))
	 (string-double (cadr (wisi-elisp-lexer--safe-intern "string-double" token-table)))
	 (string-single (cadr (wisi-elisp-lexer--safe-intern "string-single" token-table)))
	 id-alist
	 fail)
    (dolist (item punctuation-table)
      ;; check that all chars used in punctuation tokens have punctuation syntax
      (mapc (lambda (char)
	      (when (not (= ?. (char-syntax char)))
		(setq fail t)
		(message "in %s, %c does not have punctuation syntax"
			 (car item) char)))
	    (cdr item))

      ;; accumulate max length
      (when (< punct-max-length (length (cdr item)))
	(setq punct-max-length (length (cdr item))))

      ;; build id-alist
      (push item id-alist)
      )

    (when fail
      (error "aborting due to punctuation errors"))

    (when number
      (push (cons (nth 0 number) "1234") id-alist)
      (when (nth 2 number)
	(require (nth 2 number)))) ;; for number-p

    (when left-paren
      (push left-paren id-alist)
      (set (intern (cdr left-paren) keyword-table) (car left-paren)))
    (when right-paren
      (push right-paren id-alist)
      (set (intern (cdr right-paren) keyword-table) (car right-paren)))

    (when symbol
      (push (cons (car symbol) "a_bogus_identifier") id-alist))

    (when string-double
      (push (cons (car string-double) "\"\"") id-alist))

    (when string-single
      (push (cons (car string-single) "''") id-alist))

    (dolist (item keyword-table-raw)
      (push (cons (cdr item) (car item)) id-alist))

    (make-wisi-elisp-lexer
     :id-alist id-alist
     :keyword-table keyword-table
     :punctuation-table punctuation-table
     :punctuation-table-max-length punct-max-length
     :string-double-term (car string-double)
     :string-quote-escape-doubled string-quote-escape-doubled
     :string-quote-escape string-quote-escape
     :string-single-term (car string-single)
     :symbol-term (car symbol)
     :number-term (nth 0 number)
     :number-p (nth 1 number)
     )
    ))

(defun wisi-number-p (token-text)
  ;; Not ’wisi-elisp-lexer-number-p’, because this can appear in grammar files.
  "Return t if TOKEN-TEXT plus text after point matches the
syntax for a real literal; otherwise nil.  Point is after
TOKEN-TEXT; move point to just past token."
  ;; Typical literals:
  ;; 1234
  ;; 1234.5678
  ;; _not_ including non-decimal base, or underscores (see ada-wisi-number-p)
  ;;
  ;; Starts with a simple integer
  (when (string-match "^[0-9]+$" token-text)
    (when (looking-at "\\.[0-9]+")
      ;; real number
      (goto-char (match-end 0))
      (when (looking-at  "[Ee][+-][0-9]+")
        ;; exponent
        (goto-char (match-end 0))))

    t
    ))

(defun wisi-forward-token ()
  ;; Not ’wisi-elisp-lexer-forward-token’, for backward compatibility
  "Move point forward across one token, then skip whitespace and comments.
Return the corresponding token as a `wisi-tok'.
If at whitespace or comment, throw an error.
If at end of buffer, return `wisi-eoi-term'."
  (let ((start (point))
	;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
	end
	(syntax (syntax-class (syntax-after (point))))
	(first nil)
	(comment-line nil)
	(comment-end nil)
	token-id token-text line)
    (cond
     ((eobp)
      (setq token-id wisi-eoi-term))

     ((eq syntax 1)
      ;; punctuation. Find the longest matching string in wisi-punctuation-table
      (forward-char 1)
      (let ((next-point (point))
	    temp-text temp-id done)
	(while (not done)
	  (setq temp-text (buffer-substring-no-properties start (point)))
	  (setq temp-id (car (rassoc temp-text (wisi-elisp-lexer-punctuation-table wisi--lexer))))
	  (when temp-id
	    (setq token-id temp-id
		  next-point (point)))
	  (if (or
	       (eobp)
	       (= (- (point) start) (wisi-elisp-lexer-punctuation-table-max-length wisi--lexer)))
	      (setq done t)
	    (forward-char 1))
	  )
	(goto-char next-point)))

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (forward-char 1)
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id (symbol-value (intern-soft token-text (wisi-elisp-lexer-keyword-table wisi--lexer)))))

     ((eq syntax 7)
      ;; string quote, either single or double. we assume point is
      ;; before the start quote, not the end quote
      (let ((delim (char-after (point)))
	    (forward-sexp-function nil))
	(condition-case err
	    (progn
	      (forward-sexp)

	      ;; point is now after the end quote; check for an escaped quote
	      (while (or
		      (and (wisi-elisp-lexer-string-quote-escape-doubled wisi--lexer)
			   (eq (char-after (point)) delim))
		      (and (eq delim (car (wisi-elisp-lexer-string-quote-escape wisi--lexer)))
			   (eq (char-before (1- (point))) (cdr (wisi-elisp-lexer-string-quote-escape wisi--lexer)))))
		(forward-sexp))
	      (setq token-id (if (= delim ?\")
				 (wisi-elisp-lexer-string-double-term wisi--lexer)
			       (wisi-elisp-lexer-string-single-term wisi--lexer))))
	  (scan-error
	   ;; Something screwed up; we should not get here if
	   ;; syntax-propertize works properly.
	   (signal 'wisi-parse-error (format "wisi-forward-token: forward-sexp failed %s" err))
	   ))))

     ((memq syntax '(2 3 6)) ;; word, symbol, expression prefix (includes numbers)
      (skip-syntax-forward "w_'")
      (setq token-text (buffer-substring-no-properties start (point)))
      (setq token-id
	    (or (symbol-value (intern-soft (downcase token-text) (wisi-elisp-lexer-keyword-table wisi--lexer)))
		(and (functionp (wisi-elisp-lexer-number-p wisi--lexer))
		     (funcall (wisi-elisp-lexer-number-p wisi--lexer) token-text)
		     (setq token-text (buffer-substring-no-properties start (point)))
		     (wisi-elisp-lexer-number-term wisi--lexer))
		(wisi-elisp-lexer-symbol-term wisi--lexer)))
      )

     (t
      (signal 'wisi-parse-error (format "wisi-forward-token: unsupported syntax %s" syntax)))

     );; cond

    (unless token-id
      (signal 'wisi-parse-error
	      (wisi-error-msg "unrecognized token '%s'" (buffer-substring-no-properties start (point)))))

    (setq end (point))

    (forward-comment (point-max))

    (when (and (not (eq token-id wisi-eoi-term))
	       (eq wisi--parse-action 'indent))
      ;; parsing for indent; track line numbers

      (if (wisi-elisp-lexer-last-line wisi--lexer)
	  (progn
	    (setq line (wisi-elisp-lexer-last-line wisi--lexer))
	    (when (>= start (aref (wisi-elisp-lexer-line-begin wisi--lexer) line))
	      ;; first token on next non-blank line
	      (setq line (1+ line))
	      (setq first t))
	    ;; else other token on line
	    )

	;; First token on first non-comment line
	(setq line (line-number-at-pos start))
	(setq first t)
	)
      (setf (wisi-elisp-lexer-last-line wisi--lexer) line)

      ;; set comment-line, comment-end
      (when (and (< (1+ (wisi-elisp-lexer-last-line wisi--lexer)) (length (wisi-elisp-lexer-line-begin wisi--lexer)))
		 (>= (point) (aref (wisi-elisp-lexer-line-begin wisi--lexer)
				 (1+ (wisi-elisp-lexer-last-line wisi--lexer)))))
	(setq comment-line (1+ (wisi-elisp-lexer-last-line wisi--lexer)))
	(setf (wisi-elisp-lexer-last-line wisi--lexer) comment-line)
	(setq comment-end (line-end-position 0)))

      ;; count blank or comment lines following token
      (when comment-end
	(while (and (< (1+ (wisi-elisp-lexer-last-line wisi--lexer)) (length (wisi-elisp-lexer-line-begin wisi--lexer)))
		    (>= comment-end (aref (wisi-elisp-lexer-line-begin wisi--lexer) (wisi-elisp-lexer-last-line wisi--lexer))))
	  (setf (wisi-elisp-lexer-last-line wisi--lexer) (1+ (wisi-elisp-lexer-last-line wisi--lexer))))

      ))

    (make-wisi-tok
     :token token-id
     :region (cons start end)
     :line line
     :first first
     :comment-end comment-end
     :comment-line comment-line)
    ))

(defun wisi-backward-token ()
  ;; Not ’wisi-elisp-lexer-backward-token’, for backward compatibility
  "Move point backward across one token, skipping whitespace and comments.
Does _not_ handle numbers with wisi-number-p; just sees
lower-level syntax.  Return a `wisi-tok' - same structure as
wisi-forward-token, but only sets token-id and region."
  (forward-comment (- (point)))
  ;; skips leading whitespace, comment, trailing whitespace.

  ;; (info "(elisp)Syntax Table Internals" "*info elisp syntax*")
  (let ((end (point))
	(syntax (syntax-class (syntax-after (1- (point)))))
	token-id token-text)
    (cond
     ((bobp) nil)

     ((eq syntax 1)
      ;; punctuation. Find the longest matching string in wisi-lex-punctuation-table
      (backward-char 1)
      (let ((next-point (point))
	    temp-text temp-id done)
	(while (not done)
	  (setq temp-text (buffer-substring-no-properties (point) end))
	  (when (setq temp-id (car (rassoc temp-text (wisi-elisp-lexer-punctuation-table wisi--lexer))))
	    (setq token-id temp-id)
	    (setq next-point (point)))
	  (if (or
	       (bobp)
	       (= (- end (point)) (wisi-elisp-lexer-punctuation-table-max-length wisi--lexer)))
	      (setq done t)
	    (backward-char 1))
	  )
	(goto-char next-point))
      )

     ((memq syntax '(4 5)) ;; open, close parenthesis
      (backward-char 1)
      (setq token-id
	    (symbol-value
	     (intern-soft (buffer-substring-no-properties (point) end)
			  (wisi-elisp-lexer-keyword-table wisi--lexer)))))

     ((eq syntax 7)
      ;; a string quote. we assume we are after the end quote, not the start quote
      (let ((delim (char-after (1- (point))))
	    (forward-sexp-function nil))
	(forward-sexp -1)
	(setq token-id (if (= delim ?\")
			   (wisi-elisp-lexer-string-double-term wisi--lexer)
			 (wisi-elisp-lexer-string-single-term wisi--lexer)))
	))

     (t ;; assuming word or symbol syntax
      (if (zerop (skip-syntax-backward "."))
	  (skip-syntax-backward "w_'"))
      (setq token-text (buffer-substring-no-properties (point) end))
      (setq token-id
	    (or (symbol-value (intern-soft (downcase token-text) (wisi-elisp-lexer-keyword-table wisi--lexer)))
		(and (functionp (wisi-elisp-lexer-number-p wisi--lexer))
		     (funcall (wisi-elisp-lexer-number-p wisi--lexer) token-text)
		     (setq token-text (buffer-substring-no-properties (point) end))
		     (wisi-elisp-lexer-number-term wisi--lexer))
		(wisi-elisp-lexer-symbol-term wisi--lexer))))
     )

    (make-wisi-tok
     :token token-id
     :region (cons (point) end))
    ))

;;;; Debugging

(defun wisi-lex-buffer (&optional parse-action)
  ;; for timing the lexer
  (interactive)
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))

  (let* ((wisi--parse-action (or parse-action 'indent))
	 (line-count (1+ (count-lines (point-min) (point-max))))
	 )

    (cl-case wisi--parse-action
      (indent
       (setf (wisi-elisp-lexer-last-line wisi--lexer) nil)
       (setf (wisi-elisp-lexer-line-begin wisi--lexer) (wisi--set-line-begin line-count)))
      (t nil))

    (goto-char (point-min))
    (while (forward-comment 1))
    (while (not (eq wisi-eoi-term (wisi-tok-token (wisi-forward-token)))))
    ))

(defun wisi-show-token ()
  "Move forward across one keyword, show token."
  (interactive)
  (let* ((wisi--parse-action nil)
	 (token (wisi-forward-token)))
    (message "%s" token)))


(provide 'wisi-elisp-lexer)
;;; end of file
