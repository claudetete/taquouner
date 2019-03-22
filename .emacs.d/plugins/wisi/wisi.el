;;; wisi.el --- Utilities for implementing an indentation/navigation engine using a generalized LALR parser -*- lexical-binding:t -*-
;;
;; Copyright (C) 2012 - 2018  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: parser
;;  indentation
;;  navigation
;; Version: 2.0.1
;; package-requires: ((cl-lib "1.0") (emacs "25.0") (seq "2.20"))
;; URL: http://www.nongnu.org/ada-mode/wisi/wisi.html
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
;;
;;;; Design:
;;
;; 'wisi' was originally short for "wisent indentation engine", but
;; now is just a name. wisi was developed to support Emacs ada-mode
;; 5.0 indentation, font-lock, and navigation, which are parser based.
;;
;; The approach to indenting a given token is to parse the buffer,
;; computing a delta indent at each parse action.
;;
;; The parser actions also cache face and navigation information
;; as text properties on tokens in statements.
;;
;; The three reasons to run the parser (indent, face, navigate) occur
;; at different times (user indent, font-lock, user navigate), so only
;; the relevant parser actions are run.
;;
;; Since we have a cache (the text properties), we need to consider
;; when to invalidate it.  Ideally, we invalidate only when a change
;; to the buffer would change the result of a parse that crosses that
;; change, or starts after that change.  Changes in whitespace
;; (indentation and newlines) do not affect an Ada parse.  Other
;; languages are sensitive to newlines (Bash for example) or
;; indentation (Python).  Adding comments does not change a parse,
;; unless code is commented out.
;;
;; For font-lock and navigate, keeping track of the point after which
;; caches have been deleted is sufficent (see `wisi-cache-max').
;;
;; For indenting, we cache the indent for each line in a text property
;; on the newline char preceding the line. `wisi-indent-region' sets
;; the cache on all the lines computed (normally the whole buffer),
;; but performs the indent only on the lines in the indent
;; region. Subsequent calls to `wisi-indent-region' apply the cached
;; indents. Non-whitespace edits to the buffer invalidate the indent
;; caches in the edited region and after.
;;
;; See `wisi--post-change' for the details of what we check for
;; invalidating.
;;
;;;; Choice of grammar compiler and parser
;;
;; There are two other parsing engines available in Emacs:
;;
;; - SMIE
;;
;;   We don't use this because it is designed to parse small snippets
;;   of code. For Ada indentation, we always need to parse the entire
;;   buffer.
;;
;; - semantic
;;
;;   The Ada grammar as given in the Ada language reference manual is
;;   not LALR(1). So we use a generalized parser. In addition, the
;;   semantic lexer is more complex, and gives different information
;;   than we need.
;;
;; We use wisitoken wisi-generate to compile BNF to Elisp source, and
;; wisi-compile-grammar to compile that to the parser table. See
;; ada-mode info for more information on the developer tools used for
;; ada-mode and wisi.
;;
;; Alternately, to gain speed and error handling, we use wisi-generate
;; to generate Ada source, and run that in an external process. That
;; supports error correction while parsing.
;;
;;;; syntax-propertize
;;
;; `wisi-forward-token' relies on syntax properties, so
;; `syntax-propertize' must be called on the text to be lexed before
;; wisi-forward-token is called.
;;
;; Emacs >= 25 calls syntax-propertize transparently in the low-level
;; lexer functions.
;;
;; In Emacs < 25, we call syntax-propertize in wisi-setup, and in
;; `wisi--post-change'.
;;
;;;;;

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'semantic/lex)
(require 'wisi-parse-common)
(require 'wisi-elisp-lexer)
(require 'wisi-fringe)

(defcustom wisi-size-threshold 100000
  "Max size (in characters) for using wisi parser results for anything."
  :type 'integer
  :group 'wisi
  :safe 'integerp)
(make-variable-buffer-local 'wisi-size-threshold)

(defvar wisi-inhibit-parse nil
  "When non-nil, don't run the parser.
Language code can set this non-nil when syntax is known to be
invalid temporarily, or when making lots of changes.")

(defcustom wisi-disable-face nil
  "When non-nil, `wisi-setup' does not enable use of parser for font-lock.
Useful when debugging parser or parser actions."
  :type 'boolean
  :group 'wisi
  :safe 'booleanp)

(defconst wisi-error-buffer-name "*wisi syntax errors*"
  "Name of buffer for displaying syntax errors.")

(defvar wisi-error-buffer nil
  "Buffer for displaying syntax errors.")

;;;; token info cache

(defvar-local wisi-parse-failed nil
  "Non-nil when a recent parse has failed - cleared when parse succeeds.")

(defvar-local wisi--parse-try
  (list
   (cons 'face t)
   (cons 'navigate t)
   (cons 'indent t))
  "Non-nil when parse is needed - cleared when parse succeeds.")

(defun wisi-parse-try (&optional parse-action)
  (cdr (assoc (or parse-action wisi--parse-action) wisi--parse-try)))

(defun wisi-set-parse-try (value &optional parse-action)
  (setcdr (assoc (or parse-action wisi--parse-action) wisi--parse-try) value))

(defvar-local wisi--cache-max
  (list
   (cons 'face nil)
   (cons 'navigate nil)
   (cons 'indent nil))
  "Alist of maximimum position in buffer where parser text properties are valid.")

(defun wisi-cache-max (&optional parse-action)
  ;; Don't need 'wisi-set-cache-max; (move-marker (wisi-cache-max) foo) works
  (let ((mark (cdr (assoc (or parse-action wisi--parse-action) wisi--cache-max))))
    (unless (marker-position mark)
      ;; Sometimes marker gets set to <marker in no buffer>; not clear how.
      (move-marker mark (point-min)))
    mark))

(defun wisi--delete-face-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(wisi-face nil 'font-lock-face nil))
    ))

(defun wisi--delete-navigate-cache (after)
  (with-silent-modifications
    ;; This text property is 'wisi-cache', not 'wisi-navigate', for
    ;; historical reasons.
    (remove-text-properties after (point-max) '(wisi-cache nil))
    ))

(defun wisi--delete-indent-cache (after)
  (with-silent-modifications
    (remove-text-properties after (point-max) '(wisi-indent nil))
    ))

(defun wisi-invalidate-cache (action after)
  "Invalidate ACTION caches for the current buffer from AFTER to end of buffer."
  (when (< after (wisi-cache-max action))
    (when (> wisi-debug 0) (message "wisi-invalidate-cache %s:%s:%d" action (current-buffer) after))
    (cond
     ((eq 'face action)
      (wisi--delete-face-cache after))

     ((eq 'navigate action)
      ;; We goto statement start to ensure that motion within nested
      ;; structures is properly done (ie prev/next on ’elsif’ is not
      ;; set by wisi-motion-action if already set by a lower level
      ;; statement). We don’t do it for ’face or ’indent, because that
      ;; might require a parse, and they don’t care about nested
      ;; structures.
      (save-excursion
	(goto-char after)

	;; This is copied from ‘wisi-goto-statement-start’; we can’t
	;; call that because it would call ‘wisi-validate-cache’,
	;; which would call ‘wisi-invalidate-cache’; infinite loop.
	;; If this needed a navigate parse to succeed, we would not
	;; get here.
	(let ((cache (or (wisi-get-cache (point))
			 (wisi-backward-cache))))
	  (cond
	   ((null cache)
	    ;; at bob
	    nil)

	   ((eq 'statement-end (wisi-cache-class cache))
	    ;; If the change did affect part of a structure statement,
	    ;; this is a lower level statement. Otherwise, we are
	    ;; invalidating more than necessary; not a problem.
	    (wisi-goto-start cache)
	    (setq cache (wisi-backward-cache))
	    (when cache ;; else bob
	      (wisi-goto-start cache)))

	   (t
	    (wisi-goto-start cache))
	   ))

	(setq after (point)))
      (wisi--delete-navigate-cache after))

     ((eq 'indent action)
      ;; indent cache is stored on newline before line being indented.
      (setq after
	    (save-excursion
	      (goto-char after)
	      (line-beginning-position)))
      (wisi--delete-indent-cache (max 1 (1- after))))
     )
    (move-marker (wisi-cache-max action) after)
    ))

(defun wisi-reset-parser ()
  "For ’ada-reset-parser’."
  (wisi-invalidate-cache 'indent 0)
  (wisi-invalidate-cache 'face 0)
  (wisi-invalidate-cache 'navigate 0))

;; wisi--change-* keep track of buffer modifications.
;; If wisi--change-end comes before wisi--change-beg, it means there were
;; no modifications.
(defvar-local wisi--change-beg most-positive-fixnum
  "First position where a change may have taken place.")

(defvar-local wisi--change-end nil
  "Marker pointing to the last position where a change may have taken place.")

(defvar-local wisi--deleted-syntax nil
  "Worst syntax class of characters deleted in changes.
One of:
nil - no deletions since reset
0   - only whitespace or comment deleted
2   - some other syntax deleted

Set by `wisi-before-change', used and reset by `wisi--post-change'.")

(defvar-local wisi-indenting-p nil
  "Non-nil when `wisi-indent-region' is actively indenting.
Used to ignore whitespace changes in before/after change hooks.")

(defvar-local wisi--parser nil
  "Choice of wisi parser implementation; a ‘wisi-parser’ object.")

(defvar-local wisi--last-parse-action nil
  "Last value of `wisi--parse-action' when `wisi-validate-cache' was run.")

(defun wisi-before-change (begin end)
  "For `before-change-functions'."
  ;; begin . (1- end) is range of text being deleted
  (unless wisi-indenting-p
    ;; We set wisi--change-beg, -end even if only inserting, so we
    ;; don't have to do it again in wisi-after-change.
    (setq wisi--change-beg (min wisi--change-beg begin))

    (cond
     ((null wisi--change-end)
      (setq wisi--change-end (copy-marker end)))

     ((> end wisi--change-end)
      ;; `buffer-base-buffer' deals with edits in indirect buffers
      ;; created by ediff-regions-*
      (set-marker wisi--change-end end (buffer-base-buffer)))
     )

    (unless (= begin end)
      (cond
       ((or (null wisi--deleted-syntax)
	    (= 0 wisi--deleted-syntax))
	(save-excursion
	  (if (or (nth 4 (syntax-ppss begin)) ; in comment, moves point to begin
		  (= end (skip-syntax-forward " " end)));; whitespace
	      (setq wisi--deleted-syntax 0)
	    (setq wisi--deleted-syntax 2))))

       (t
	;; wisi--deleted-syntax is 2; no change.
	)
       ))))

(defun wisi-after-change (begin end _length)
  "For `after-change-functions'"
  ;; begin . end is range of text being inserted (empty if equal);
  ;; length is the size of the deleted text.
  ;;
  ;; This change might be changing to/from a keyword; trigger
  ;; font-lock. See test/ada_mode-interactive_common.adb Obj_1.
  (unless wisi-indenting-p
    (save-excursion
      (let (word-end)
	(goto-char end)
	(skip-syntax-forward "w_")
	(setq word-end (point))
	(goto-char begin)
	(skip-syntax-backward "w_")
	(with-silent-modifications
	  (remove-text-properties (point) word-end '(font-lock-face nil fontified nil)))
	)
      )))

(defun wisi--post-change (begin end)
  "Update wisi text properties for changes in region BEG END."
  ;; (syntax-ppss-flush-cache begin) is in before-change-functions

  ;; see comments above on syntax-propertize
  (when (< emacs-major-version 25) (syntax-propertize end))

  ;; Remove caches on inserted text, which could have caches from
  ;; before the failed parse (or another buffer), and are in any case
  ;; invalid. No point in removing 'fontified; that's handled by
  ;; jit-lock.

  (with-silent-modifications
    (remove-text-properties begin end '(wisi-cache nil font-lock-face nil)))

  (save-excursion
    (let ((need-invalidate t)
	  (done nil)
	  ;; non-nil if require a parse because the syntax may have
	  ;; changed.

	  (begin-state (syntax-ppss begin))
	  (end-state (syntax-ppss end)))
	  ;; (info "(elisp)Parser State")
	  ;; syntax-ppss has moved point to "end"; might be eob.

      ;; consider deletion
      (cond
       ((null wisi--deleted-syntax)
	;; no deletions
	)

       ((= 0 wisi--deleted-syntax)
	;; Only deleted whitespace; may have joined two words
	(when
	    (and (= begin end) ;; no insertions
		 (or
		  (= (point-min) begin)
		  (= 0 (syntax-class (syntax-after (1- begin))))
		  (= (point-max) end)
		  (= 0 (syntax-class (syntax-after end)))))
	  ;; More whitespace on at least one side of deletion; did not
	  ;; join two words.
	  (setq need-invalidate nil)
	  (setq done t)
	  ))

       (t
	;; wisi--deleted-syntax is 2; need invalidate and parse for all
	;; parse actions
	(setq done t)
	))

      (setq wisi--deleted-syntax nil)

      (unless done
	;; consider insertion
	(cond
	 ((= begin end)
	  ;; no insertions
	  nil)

	 ((and
	   (nth 3 begin-state);; in string
	   (nth 3 end-state)
	   (= (nth 8 begin-state) (nth 8 end-state)));; no intervening non-string
	  (setq need-invalidate nil))

	 ((and
	   (nth 4 begin-state) ; in comment
	   (nth 4 end-state)
	   (= (nth 8 begin-state) (nth 8 end-state))) ;; no intervening non-comment
	  (setq need-invalidate nil))

	 ((and
	   (or
	    (= (point-min) begin)
	    (= 0 (syntax-class (syntax-after (1- begin)))); whitespace
	    (= (point-max) end)
	    (= 0 (syntax-class (syntax-after end))))
	   (progn
	     (goto-char begin)
	     (= (- end begin) (skip-syntax-forward " " end))
	     ))
	  ;; Inserted only whitespace, there is more whitespace on at
	  ;; least one side, and we are not in a comment or string
	  ;; (checked above).  This may affect indentation, but not
	  ;; the indentation cache.
	  (setq need-invalidate nil))
	 ))

      (when need-invalidate
	(wisi-set-parse-try t 'face)
	(wisi-set-parse-try t 'navigate)
	(wisi-set-parse-try t 'indent)

	(wisi-invalidate-cache 'face begin)
	(wisi-invalidate-cache 'navigate begin)
	(wisi-invalidate-cache 'indent begin))
      )))

(defun wisi-goto-error ()
  "Move point to position in last error message (if any)."
  (cond
   ((wisi-parser-parse-errors wisi--parser)
    (let ((data (car (wisi-parser-parse-errors wisi--parser))))
      (cond
       ((wisi--parse-error-pos data)
	(push-mark)
	(goto-char (wisi--parse-error-pos data)))

       ((string-match ":\\([0-9]+\\):\\([0-9]+\\):" (wisi--parse-error-message data))
	(let* ((msg (wisi--parse-error-message data))
	       (line (string-to-number (match-string 1 msg)))
	       (col (string-to-number (match-string 2 msg))))
	  (push-mark)
	  (goto-char (point-min))
	  (condition-case nil
	      (progn
		;; line can be wrong if parser screws up, or user edits buffer
		(forward-line (1- line))
		(forward-char col))
	    (error
	     ;; just stay at eob.
	     nil))))
       )))
   ((wisi-parser-lexer-errors wisi--parser)
    (push-mark)
    (goto-char (wisi--lexer-error-pos (car (wisi-parser-lexer-errors wisi--parser)))))
   ))

(defun wisi-show-parse-error ()
  "Show current wisi-parse errors."
  (interactive)
  (cond
   ((or (wisi-parser-lexer-errors wisi--parser)
	(wisi-parser-parse-errors wisi--parser))
    (if (and (= 1 (+ (length (wisi-parser-lexer-errors wisi--parser))
		     (length (wisi-parser-parse-errors wisi--parser))))
	     (or (and (wisi-parser-parse-errors wisi--parser)
		      (not (wisi--parse-error-repair (car (wisi-parser-parse-errors wisi--parser)))))
		 (and (wisi-parser-lexer-errors wisi--parser)
		      (not (wisi--lexer-error-inserted (car (wisi-parser-lexer-errors wisi--parser)))))))
	;; There is exactly one error; if there is error correction
	;; information, use a ’compilation’ buffer, so
	;; *-fix-compiler-error will call
	;; wisi-repair-error. Otherwise, just position cursor at
	;; error.
	(progn
	  (wisi-goto-error)
	  (message (or (and (wisi-parser-parse-errors wisi--parser)
			    (wisi--parse-error-message (car (wisi-parser-parse-errors wisi--parser))))
		       (and (wisi-parser-lexer-errors wisi--parser)
			    (wisi--lexer-error-message (car (wisi-parser-lexer-errors wisi--parser)))))
		   ))

      ;; else show all errors in a ’compilation’ buffer
      (setq wisi-error-buffer (get-buffer-create wisi-error-buffer-name))

      (let ((lexer-errs (nreverse (cl-copy-seq (wisi-parser-lexer-errors wisi--parser))))
	    (parse-errs (nreverse (cl-copy-seq (wisi-parser-parse-errors wisi--parser)))))
	(with-current-buffer wisi-error-buffer
	  (compilation-mode)
	  (setq next-error-last-buffer (current-buffer))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  ;; compilation-nex-error-function assumes there is not an
	  ;; error at point min, so we need a comment.
	  (insert "wisi syntax errors")
	  (newline)
	  (dolist (err lexer-errs)
	    (insert (wisi--lexer-error-message err))
	    (put-text-property (line-beginning-position) (1+ (line-beginning-position)) 'wisi-error-data err)
	    (newline 2))
	  (dolist (err parse-errs)
	    (insert (wisi--parse-error-message err))
	    (put-text-property (line-beginning-position) (1+ (line-beginning-position)) 'wisi-error-data err)
	    (newline 2))
	  (compilation--flush-parse (point-min) (point-max))
	  (compilation--ensure-parse (point-max))
	  (when compilation-filter-hook
	    (let ((compilation-filter-start (point-min)))
	      (run-hooks 'compilation-filter-hook)))

	  (setq buffer-read-only t)
	  (goto-char (point-min)))

	(display-buffer wisi-error-buffer
			(cons #'display-buffer-at-bottom
			      (list (cons 'window-height #'shrink-window-if-larger-than-buffer))))
	(next-error))
      ))

   ((wisi-parse-try wisi--last-parse-action)
    (message "need parse"))

   (t
    (message "parse succeeded"))
   ))

(defun wisi-kill-parser ()
  "Kill the background process running the parser for the current buffer.
Usefull if the parser appears to be hung."
  (interactive)
  (wisi-parse-kill wisi--parser)
  ;; also force re-parse
  (dolist (parse-action '(face navigate indent))
    (wisi-set-parse-try t parse-action)
    (move-marker (wisi-cache-max parse-action) (point-max));; force delete caches
    (wisi-invalidate-cache parse-action (point-min)))
  )

(defun wisi--run-parse ()
  "Run the parser."
  (unless (buffer-narrowed-p)
    (let ((msg (when (> wisi-debug 0)
		 (format "wisi: parsing %s %s:%d ..."
			 wisi--parse-action
			 (buffer-name)
			 (line-number-at-pos (point))))))
      (when (> wisi-debug 0)
	(message msg))

      (setq wisi--last-parse-action wisi--parse-action)

      (unless (eq wisi--parse-action 'face)
	(when (buffer-live-p wisi-error-buffer)
	  (with-current-buffer wisi-error-buffer
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (setq buffer-read-only t))))

      (condition-case-unless-debug err
	  (save-excursion
	    (wisi-parse-current wisi--parser)
	    (setq wisi-parse-failed nil)
	    (move-marker (wisi-cache-max) (point))
	    )
	(wisi-parse-error
	 (cl-ecase wisi--parse-action
	   (face
	    ;; caches set by failed parse are ok
	    (wisi--delete-face-cache (wisi-cache-max)))

	   (navigate
	    ;; parse partially resets caches before and after wisi-cache-max
	    (move-marker (wisi-cache-max) (point-min))
	    (wisi--delete-navigate-cache (point-min)))

	   (indent
	    ;; parse does not set caches; see `wisi-indent-region'
	    nil))
	 (setq wisi-parse-failed t)
	 ;; parser should have stored this error message in parser-error-msgs
	 )
	(error
	 ;; parser failed for other reason
	 (setq wisi-parse-failed t)
	 (signal (car err) (cdr err)))
	)

      (wisi-fringe-display-errors
       (append
	(seq-map (lambda (err) (wisi--lexer-error-pos err)) (wisi-parser-lexer-errors wisi--parser))
	(seq-map (lambda (err) (wisi--parse-error-pos err)) (wisi-parser-parse-errors wisi--parser))))

      (when (> wisi-debug 0)
	(if (or (wisi-parser-lexer-errors wisi--parser)
		(wisi-parser-parse-errors wisi--parser))
	    (progn
	      (message "%s error" msg)
	      (wisi-goto-error)
	      (error (or (and (wisi-parser-lexer-errors wisi--parser)
			      (wisi--lexer-error-message (car (wisi-parser-lexer-errors wisi--parser))))
			 (and (wisi-parser-parse-errors wisi--parser)
			      (wisi--parse-error-message (car (wisi-parser-parse-errors wisi--parser))))
			 )))

	  ;; no error
	  (message "%s done" msg))
	))))

(defun wisi--check-change ()
  "Process `wisi--change-beg', `wisi--change-end'.
`wisi--parse-action' must be bound."
  (when (and wisi--change-beg
	     wisi--change-end
	     (<= wisi--change-beg wisi--change-end))
    (wisi--post-change wisi--change-beg (marker-position wisi--change-end))
    (setq wisi--change-beg most-positive-fixnum)
    (move-marker wisi--change-end (point-min))
    ))

(defun wisi-validate-cache (pos error-on-fail parse-action)
  "Ensure cached data for PARSE-ACTION is valid at least up to POS in current buffer."
  (if (< (point-max) wisi-size-threshold)
      (let ((wisi--parse-action parse-action))
	(wisi--check-change)

	;; Now we can rely on wisi-cache-max.

	;; If wisi-cache-max = pos, then there is no cache at pos; need parse
	(when (and (not wisi-inhibit-parse)
		   (wisi-parse-try)
		   (<= (wisi-cache-max) pos))

	  ;; Don't keep retrying failed parse until text changes again.
	  (wisi-set-parse-try nil)

	  (wisi--run-parse))

	;; We want this error even if we did not try to parse; it means
	;; the parse results are not valid.
	(when (and error-on-fail wisi-parse-failed)
	  (error "parse %s failed" parse-action))
	)
    (when (> wisi-debug 0)
      (message "parse skipped due to ‘wisi-size-threshold’"))))

(defun wisi-fontify-region (_begin end)
  "For `jit-lock-functions'."
  (wisi-validate-cache end nil 'face))

(defun wisi-get-containing-cache (cache)
  "Return cache from (wisi-cache-containing CACHE)."
  (when cache
    (let ((containing (wisi-cache-containing cache)))
      (and containing
	   (wisi-get-cache containing)))))

(defun wisi-cache-text (cache)
  "Return property-less buffer substring designated by cache.
Point must be at cache."
  (buffer-substring-no-properties (point) (+ (point) (wisi-cache-last cache))))

;;;; navigation

(defun wisi-forward-find-class (class limit)
  "Search at point or forward for a token that has a cache with CLASS.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-forward-cache))))
    (while (not (eq class (wisi-cache-class cache)))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with class %s not found" class)))
    cache))

(defun wisi-forward-find-token (token limit &optional noerror)
  "Search forward for TOKEN.
If point is at a matching token, return that token.  TOKEN may be
a list; stop on any member of the list.  Return `wisi-tok'
struct, or if LIMIT (a buffer position) is reached, then if
NOERROR is nil, throw an error, if non-nil, return nil."
  (let ((token-list (cond
		     ((listp token) token)
		     (t (list token))))
	(tok (wisi-forward-token))
	(done nil))
    (while (not (or done
		    (memq (wisi-tok-token tok) token-list)))
      (setq tok (wisi-forward-token))
      (when (or (>= (point) limit)
		(eobp))
	(goto-char limit)
	(setq tok nil)
	(if noerror
	    (setq done t)
	  (error "token %s not found" token))))
    tok))

(defun wisi-forward-find-cache-token (ids limit)
  "Search forward for a cache with token in IDS (a list of token ids).
Return cache, or nil if at LIMIT or end of buffer."
  (let ((cache (wisi-forward-cache)))
    (while (and (< (point) limit)
		(not (eobp))
		(not (memq (wisi-cache-token cache) ids)))
      (setq cache (wisi-forward-cache)))
    cache))

(defun wisi-forward-find-nonterm (nonterm limit)
  "Search forward for a token that has a cache with NONTERM.
NONTERM may be a list; stop on any cache that has a member of the list.
Return cache, or nil if at end of buffer.
If LIMIT (a buffer position) is reached, throw an error."
  (let ((nonterm-list (cond
		       ((listp nonterm) nonterm)
		       (t (list nonterm))))
	(cache (wisi-forward-cache)))
    (while (not (memq (wisi-cache-nonterm cache) nonterm-list))
      (setq cache (wisi-forward-cache))
      (when (>= (point) limit)
	(error "cache with nonterm %s not found" nonterm)))
    cache))

(defun wisi-goto-cache-next (cache)
  (goto-char (wisi-cache-next cache))
  (wisi-get-cache (point))
  )

(defun wisi-forward-statement-keyword ()
  "If not at a cached token, move forward to next
cache. Otherwise move to cache-next, or cache-end, or next cache
if both nil.  Return cache found."
  (unless (eobp)
    (wisi-validate-cache (point-max) t 'navigate) ;; ensure there is a next cache to move to
    (let ((cache (wisi-get-cache (point))))
      (if (and cache
	       (not (eq (wisi-cache-class cache) 'statement-end)))
	  (let ((next (or (wisi-cache-next cache)
			  (wisi-cache-end cache))))
	    (if next
		(goto-char next)
	      (wisi-forward-cache)))
	(wisi-forward-cache))
      )
    (wisi-get-cache (point))
    ))

(defun wisi-backward-statement-keyword ()
  "If not at a cached token, move backward to prev
cache. Otherwise move to cache-prev, or prev cache if nil."
  (wisi-validate-cache (point) t 'navigate)
  (let ((cache (wisi-get-cache (point)))
	prev)
    (when cache
      (setq prev (wisi-cache-prev cache))
      (unless prev
	(unless (eq 'statement-start (wisi-cache-class cache))
	  (setq prev (wisi-cache-containing cache)))))
    (if prev
	(goto-char prev)
      (wisi-backward-cache))
  ))

(defun wisi-forward-sexp (&optional arg)
  "For `forward-sexp-function'."
  (interactive "^p")
  (or arg (setq arg 1))
  (cond
   ((and (> arg 0) (= 4 (syntax-class (syntax-after (point)))))  ;; on open paren
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (< arg 0) (= 5 (syntax-class (syntax-after (1- (point)))))) ;; after close paren
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (> arg 0) (= 7 (syntax-class (syntax-after (point)))))  ;; on (open) string quote
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   ((and (< arg 0) (= 7 (syntax-class (syntax-after (1- (point)))))) ;; after (close) string quote
    (let ((forward-sexp-function nil))
      (forward-sexp arg)))

   (t
    (dotimes (_i (abs arg))
      (if (> arg 0)
	  (wisi-forward-statement-keyword)
	(wisi-backward-statement-keyword))))
   ))

(defun wisi-goto-containing (cache &optional error)
  "Move point to containing token for CACHE, return cache at that point.
If ERROR, throw error when CACHE has no container; else return nil."
  (cond
   ((and (markerp (wisi-cache-containing cache))

	 (not (= (wisi-cache-containing cache) (point))))
    ;; This check is only needed if some cache points to itself as a
    ;; container. Apparently that happend once that I caught in the
    ;; debugger; emacs hung because we got here in the font-lock
    ;; timer.

    (goto-char (wisi-cache-containing cache))
    (wisi-get-cache (point)))
   (t
    (when error
      (error "already at outermost containing token")))
   ))

(defun wisi-goto-containing-paren (cache)
  "Move point to just after the open-paren containing CACHE.
Return cache for paren, or nil if no containing paren."
  (while (and cache
	      (not (eq (wisi-cache-class cache) 'open-paren)))
    (setq cache (wisi-goto-containing cache)))
  (when cache
    (forward-char 1))
  cache)

(defun wisi-goto-start (cache)
  "Move point to containing ancestor of CACHE that has class statement-start.
Return start cache."
  ;; cache nil at bob, or on cache in partially parsed statement
  (while (and cache
	      (not (eq (wisi-cache-class cache) 'statement-start)))
    (setq cache (wisi-goto-containing cache)))
  cache)

(defun wisi-goto-end-1 (cache)
  (goto-char (wisi-cache-end cache)))

(defun wisi-goto-statement-start ()
  "Move point to token at start of statement point is in or after.
Return start cache."
  (interactive)
  (wisi-validate-cache (point) t 'navigate)
  (wisi-goto-start (or (wisi-get-cache (point))
		       (wisi-backward-cache))))

(defun wisi-goto-statement-end ()
  "Move point to token at end of statement point is in or before."
  (interactive)
  (wisi-validate-cache (point) t 'navigate)
  (let ((cache (or (wisi-get-cache (point))
		   (wisi-forward-cache))))
    (when (wisi-cache-end cache)
      ;; nil when cache is statement-end
      (wisi-goto-end-1 cache))
    ))

(defun wisi-next-statement-cache (cache)
  "Move point to CACHE-next, return cache; error if nil."
  (when (not (markerp (wisi-cache-next cache)))
    (error "no next statement cache"))
  (goto-char (wisi-cache-next cache))
  (wisi-get-cache (point)))

(defun wisi-prev-statement-cache (cache)
  "Move point to CACHE-prev, return cache; error if nil."
  (when (not (markerp (wisi-cache-prev cache)))
    (error "no prev statement cache"))
  (goto-char (wisi-cache-prev cache))
  (wisi-get-cache (point)))

;;;; indentation

(defun wisi-comment-indent ()
  "For `comment-indent-function'. Indent single line comment to
the comment on the previous line."
  ;; Called from `comment-indent', either to insert a new comment, or
  ;; to indent the first line of an existing one.  In either case, the
  ;; comment may be after code on the same line.  For an existing
  ;; comment, point is at the start of the starting delimiter.
  (or
   (save-excursion
     ;; Check for a preceding comment line; fail if comment follows code.
     (when (forward-comment -1)
       ;; For the case:
       ;;
       ;; code;-- comment
       ;;
       ;; point is on '--', and 'forward-comment' does not move point,
       ;; returns nil.
       (when (looking-at comment-start)
         (current-column))))

   (save-excursion
     (back-to-indentation)
     (if (looking-at comment-start)
         ;; An existing comment, no code preceding comment, and
         ;; no comment on preceding line. Return nil, so
         ;; `comment-indent' will call `indent-according-to-mode'
         nil

       ;; A comment after code on the same line.
       comment-column))
   ))

(defun wisi-indent-statement ()
  "Indent region given by `wisi-goto-start', `wisi-cache-end'."
  (wisi-validate-cache (point) t 'navigate)

  (save-excursion
    (let ((cache (or (wisi-get-cache (point))
		     (wisi-backward-cache))))
      (when cache
	;; can be nil if in header comment
	(let ((start (progn (wisi-goto-start cache) (point)))
	      (end (if (wisi-cache-end cache)
			 ;; nil when cache is statement-end
			 (wisi-cache-end cache)
		       (point))))
	  (indent-region start end)
	  ))
      )))

(defvar-local wisi-indent-calculate-functions nil
  "Functions to compute indentation special cases.
Called with point at current indentation of a line; return
indentation column, or nil if function does not know how to
indent that line. Run after parser indentation, so other lines
are indented correctly.")

(defvar-local wisi-post-indent-fail-hook
  "Function to reindent portion of buffer.
Called from `wisi-indent-region' when a parse succeeds after
failing; assumes user was editing code that is now syntactically
correct. Must leave point at indentation of current line.")

(defvar-local wisi-indent-failed nil
  "Non-nil when wisi-indent-region fails due to parse failing; cleared when indent succeeds.")

(defvar-local wisi-indent-region-fallback 'wisi-indent-region-fallback-default
  "Function to compute indent for lines in region when wisi parse fails.
Called with BEGIN END.")

(defun wisi-indent-region-fallback-default (begin end)
  ;; Assume there is no indent info at point; user is editing. Indent
  ;; to previous lines.
  (goto-char begin)
  (forward-line -1);; safe at bob
  (back-to-indentation)
  (let ((col (current-column)))
    (while (and (not (eobp))
		(< (point) end))
      (forward-line 1)
      (indent-line-to col)
      (when (bobp)
	;; single line in buffer; terminate loop
	(goto-char (point-max))))))

(defun wisi-indent-region (begin end)
  "For `indent-region-function', using the wisi indentation engine."
  (let ((wisi--parse-action 'indent)
	(parse-required nil)
	(end-mark (copy-marker end))
	(prev-indent-failed wisi-indent-failed))

    (wisi--check-change)

    ;; Always indent the line containing BEGIN.
    (save-excursion
      (goto-char begin)
      (setq begin (line-beginning-position))

      (when (bobp) (forward-line))
      (while (and (not parse-required)
		  (<= (point) end)
		  (not (eobp)))
	(unless (get-text-property (1- (point)) 'wisi-indent)
	  (setq parse-required t))
	(forward-line))
      )

    ;; A parse either succeeds and sets the indent cache on all
    ;; lines in the buffer, or fails and leaves valid caches
    ;; untouched.
    (when (and parse-required
	       (wisi-parse-try))

      (wisi-set-parse-try nil)
      (wisi--run-parse)

      ;; If there were errors corrected, the indentation is
      ;; potentially ambiguous; see test/ada_mode-interactive_2.adb
      (setq wisi-indent-failed (< 0 (+ (length (wisi-parser-lexer-errors wisi--parser))
				       (length (wisi-parser-parse-errors wisi--parser)))))
      )

    (if wisi-parse-failed
	(progn
	  ;; primary indent failed
	  (setq wisi-indent-failed t)
	  (when (functionp wisi-indent-region-fallback)
	    (funcall wisi-indent-region-fallback begin end)))

      (save-excursion
	;; Apply cached indents.
	(goto-char begin)
	(let ((wisi-indenting-p t))
	  (while (and (not (eobp))
		      (<= (point) end-mark)) ;; end-mark can be at the start of an empty line
	    (indent-line-to (if (bobp) 0 (get-text-property (1- (point)) 'wisi-indent)))
	    (forward-line 1)))

	;; Run wisi-indent-calculate-functions
	(when wisi-indent-calculate-functions
	  (goto-char begin)
	  (while (and (not (eobp))
		      (< (point) end-mark))
	    (back-to-indentation)
	    (let ((indent
		   (run-hook-with-args-until-success 'wisi-indent-calculate-functions)))
	      (when indent
		(indent-line-to indent)))

	    (forward-line 1)))

	(when
	    (and prev-indent-failed
		 (not wisi-indent-failed))
	  ;; Previous parse failed or indent was potentially
	  ;; ambiguous, this one is not.
	  (goto-char end-mark)
	  (run-hooks 'wisi-post-indent-fail-hook))
	))
    ))

(defun wisi-indent-line ()
  "For `indent-line-function'."
  (let ((savep (copy-marker (point)))
	(to-indent nil))
    (back-to-indentation)
    (when (>= (point) savep)
      (setq to-indent t))

    (wisi-indent-region (line-beginning-position) (line-end-position))

    (goto-char savep)
    (when to-indent (back-to-indentation))
    ))

(defun wisi-repair-error-1 (data)
  "Repair error reported in DATA (a ’wisi--parse-error’ or ’wisi--lexer-error’)"
  (let ((wisi--parse-action 'navigate) ;; tell wisi-forward-token not to compute indent stuff.
	tok-2)
    (cond
     ((wisi--lexer-error-p data)
      (goto-char (1+ (wisi--lexer-error-pos data)))
      (insert (wisi--lexer-error-inserted data)))
     ((wisi--parse-error-p data)
      (dolist (repair (wisi--parse-error-repair data))
	(goto-char (wisi--parse-error-repair-pos repair))
	(dolist (tok-1 (wisi--parse-error-repair-deleted repair))
	  (setq tok-2 (wisi-forward-token))
	  (if (eq tok-1 (wisi-tok-token tok-2))
	      (delete-region (car (wisi-tok-region tok-2)) (cdr (wisi-tok-region tok-2)))
	    (error "mismatched tokens: %d: parser %s, buffer %s %s"
		   (point) tok-1 (wisi-tok-token tok-2) (wisi-tok-region tok-2))))

	(dolist (id (wisi--parse-error-repair-inserted repair))
	  (insert (cdr (assoc id (wisi-elisp-lexer-id-alist wisi--lexer))))
	  (insert " "))
	))
     )))

(defun wisi-repair-error ()
  "Repair the current error."
  (interactive)
  (let ((wisi-inhibit-parse t)) ;; don’t let the error list change while we are processing it.
    (if (= 1 (+ (length (wisi-parser-lexer-errors wisi--parser))
		(length (wisi-parser-parse-errors wisi--parser))))
	(progn
	  (wisi-goto-error)
	  (wisi-repair-error-1 (or (car (wisi-parser-lexer-errors wisi--parser))
				   (car (wisi-parser-parse-errors wisi--parser)))))
      (if (buffer-live-p wisi-error-buffer)
	  (let ((err
		 (with-current-buffer wisi-error-buffer
		   ;; FIXME: ensure at beginning of error message line.
		   (get-text-property (point) 'wisi-error-data))))
	    (wisi-repair-error-1 err))
	(error "no current error found")
	))))

(defun wisi-repair-errors (&optional beg end)
  "Repair errors reported by last parse.
If non-nil, only repair errors in BEG END region."
  (interactive)
  (let ((wisi-inhibit-parse t)) ;; don’t let the error list change while we are processing it.
    (dolist (data (wisi-parser-lexer-errors wisi--parser))
      (when (or (null beg)
		(and (not (= 0 (wisi--lexer-error-inserted data)))
		     (wisi--lexer-error-pos data)
		     (<= beg (wisi--lexer-error-pos data))
		     (<= (wisi--lexer-error-pos data) end)))
	(wisi-repair-error-1 data)))

    (dolist (data (wisi-parser-parse-errors wisi--parser))
      (when (or (null beg)
		(and (wisi--parse-error-pos data)
		     (<= beg (wisi--parse-error-pos data))
		     (<= (wisi--parse-error-pos data) end)))
	(wisi-repair-error-1 data)))
    ))

;;;; debugging

(defun wisi-debug-keys ()
  "Add debug key definitions to `global-map'."
  (interactive)
  (define-key global-map "\M-h" 'wisi-show-containing-or-previous-cache)
  (define-key global-map "\M-i" 'wisi-show-indent)
  (define-key global-map "\M-j" 'wisi-show-cache)
  )

(defun wisi-parse-buffer (&optional parse-action)
  (interactive)
  (unless parse-action (setq parse-action 'indent))
  (wisi-set-parse-try t parse-action)
  (move-marker (wisi-cache-max parse-action) (point-max));; force delete caches
  (wisi-invalidate-cache parse-action (point-min))

  (cl-ecase parse-action
    (face
     (with-silent-modifications
       (remove-text-properties
	(point-min) (point-max)
	(list
	 'font-lock-face nil
	 'fontified nil)))
     (wisi-validate-cache (point-max) t parse-action)
     (when (fboundp 'font-lock-ensure) (font-lock-ensure))) ;; emacs < 25

    (navigate
     (wisi-validate-cache (point-max) t parse-action))

    (indent
     (wisi-indent-region (point-min) (point-max)))
    ))

(defun wisi-time (func count &optional report-wait-time)
  "call FUNC COUNT times, show total time"
  (interactive "afunction \nncount ")

  (let ((start-time (float-time))
	(start-gcs gcs-done)
	(cum-wait-time 0.0)
        (i 0)
        diff-time
	diff-gcs)
    (while (not (eq (1+ count) (setq i (1+ i))))
      (save-excursion
        (funcall func))
      (when report-wait-time
	(setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
    (setq diff-time (- (float-time) start-time))
    (setq diff-gcs (- gcs-done start-gcs))
    (if report-wait-time
	(progn
	  (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs %d responses %f wait"
		   diff-time
		   diff-gcs
		   (/ diff-time count)
		   (/ (float diff-gcs) count)
		   (wisi-process--parser-response-count wisi--parser)
		   (/ cum-wait-time count)))

      (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs"
	       diff-time
	       diff-gcs
	       (/ diff-time count)
	       (/ (float diff-gcs) count))
      ))
  nil)

(defun wisi-time-indent-middle-line-cold-cache (count &optional report-wait-time)
  (goto-char (point-min))
  (forward-line (1- (/ (count-lines (point-min) (point-max)) 2)))
  (let ((cum-wait-time 0.0))
    (wisi-time
     (lambda ()
       (wisi-set-parse-try t 'indent)
       (move-marker (wisi-cache-max 'indent) (point-max));; force delete caches
       (wisi-invalidate-cache 'indent (point-min))
       (wisi-indent-line)
       (when (wisi-process--parser-p wisi--parser)
	 (setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
     count
     report-wait-time)
    ))

(defun wisi-time-indent-middle-line-warm-cache (count)
  (wisi-set-parse-try t 'indent)
  (move-marker (wisi-cache-max 'indent) (point-max));; force delete caches
  (wisi-invalidate-cache 'indent (point-min))
  (goto-char (point-min))
  (forward-line (/ (count-lines (point-min) (point-max)) 2))
  (wisi-indent-line)
  (wisi-time #'wisi-indent-line count))

(defun wisi-show-indent ()
  "Show indent cache for current line."
  (interactive)
  (message "%s" (get-text-property (1- (line-beginning-position)) 'wisi-indent)))

(defun wisi-show-cache ()
  "Show navigation and face caches, and applied faces, at point."
  (interactive)
  (message "%s:%s:%s:%s"
	   (wisi-get-cache (point))
	   (get-text-property (point) 'wisi-face)
	   (get-text-property (point) 'face)
	   (get-text-property (point) 'font-lock-face)
	   ))

(defun wisi-show-containing-or-previous-cache ()
  (interactive)
  (let ((cache (wisi-get-cache (point))))
    (if cache
	(message "containing %s" (wisi-goto-containing cache t))
      (message "previous %s" (wisi-backward-cache)))
    ))

(defun wisi-show-cache-max (action)
  (push-mark)
  (goto-char (wisi-cache-max action)))

;;;;; setup

(cl-defun wisi-setup (&key indent-calculate post-indent-fail parser lexer)
  "Set up a buffer for parsing files with wisi."
  (when wisi--parser
    (wisi-kill-parser))

  (setq wisi--parser parser)
  (setq wisi--lexer lexer)

  (setq wisi--cache-max
	(list
	 (cons 'face (copy-marker (point-min)))
	 (cons 'navigate (copy-marker (point-min)))
	 (cons 'indent (copy-marker (point-min)))))

  (setq wisi--parse-try
	(list
	 (cons 'face t)
	 (cons 'navigate t)
	 (cons 'indent t)))

  ;; file local variables may have added opentoken, gnatprep
  (setq wisi-indent-calculate-functions (append wisi-indent-calculate-functions indent-calculate))
  (set (make-local-variable 'indent-line-function) #'wisi-indent-line)
  (set (make-local-variable 'indent-region-function) #'wisi-indent-region)
  (set (make-local-variable 'forward-sexp-function) #'wisi-forward-sexp)

  (setq wisi-post-indent-fail-hook post-indent-fail)
  (setq wisi-indent-failed nil)

  (add-hook 'before-change-functions #'wisi-before-change 'append t)
  (add-hook 'after-change-functions #'wisi-after-change nil t)
  (setq wisi--change-end (copy-marker (point-min) t))

  ;; See comments above on syntax-propertize.
  (when (< emacs-major-version 25) (syntax-propertize (point-max)))

  ;; In Emacs >= 26, ‘run-mode-hooks’ (in the major mode function)
  ;; runs ‘hack-local-variables’ after ’*-mode-hooks’; we need
  ;; ‘wisi-post-local-vars’ to run after ‘hack-local-variables’.
  (add-hook 'hack-local-variables-hook 'wisi-post-local-vars nil t)
  )

(defun wisi-post-local-vars ()
  "See wisi-setup."
  (setq hack-local-variables-hook (delq 'wisi-post-local-vars hack-local-variables-hook))

  (unless wisi-disable-face
    (jit-lock-register #'wisi-fontify-region)))


(provide 'wisi)
;;; wisi.el ends here
