;;; wisi-process-parse.el --- interface to external parse program
;;
;; Copyright (C) 2014, 2017, 2018 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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

(require 'cl-lib)
(require 'wisi-parse-common)

(defconst wisi-process-parse-prompt "^;;> "
  "Regexp matching executable prompt; indicates previous command is complete.")

(defconst wisi-process-parse-quit-cmd "004quit\n"
  "Command to external process telling it to quit.")

(defvar wisi-process-parse-debug 0)

;;;;; sessions

;; The executable builds internal parser structures on startup,
;; then runs a loop, waiting for parse requests.
;;
;; We only need one process per language; there is no persistent state
;; in the process between parses, and processes are too heavy-weight
;; to have one per buffer. We use a global alist of parser objects to
;; find the right one for the current buffer.

(cl-defstruct (wisi-process--parser (:include wisi-parser))
  (label nil)             ;; string uniquely identifying parser
  (exec-file nil) 	  ;; absolute file name of executable
  (exec-opts nil)         ;; list of process start options for executable
  (token-table nil)       ;; vector of token symbols, indexed by integer
  (face-table nil) 	  ;; vector of face symbols, indexed by integer
  (busy nil)              ;; t while parser is active
  (process nil) 	  ;; running *_wisi_parse executable
  (buffer nil) 		  ;; receives output of executable
  line-begin              ;; vector of beginning-of-line positions in buffer
  (total-wait-time 0.0)   ;; total time during last parse spent waiting for subprocess output.
  (response-count 0)      ;; responses received from subprocess during last parse; for profiling.
  )

(defvar wisi-process--alist nil
  "Alist mapping string label to ‘wisi-process--session’ struct")

(defgroup wisi nil
  "Options for Wisi package."
  :group 'programming)

(defcustom wisi-process-time-out 1.0
  "Time out waiting for parser response. An error occurs if there
  is no response from the parser after waiting this amount 5
  times."
  :type 'float
  :safe 'floatp)
(make-variable-buffer-local 'wisi-process-time-out)

;;;###autoload
(defun wisi-process-parse-get (parser)
  "Return a ‘wisi-process--parser’ object matching PARSER label.
If label found in ‘wisi-process--alist’, return that.
Otherwise add PARSER to ‘wisi-process--alist’, return it."
  (or (cdr (assoc (wisi-process--parser-label parser) wisi-process--alist))
      (let ((exec-file (locate-file (wisi-process--parser-exec-file parser) exec-path '("" ".exe"))))

	(unless exec-file
	  (error "%s not found on `exec-path'" (wisi-process--parser-exec-file parser)))

	(push (cons (wisi-process--parser-label parser) parser) wisi-process--alist)

	parser
     )))

(defun wisi-process-parse-set-exec (label exec-file)
  "Change the EXEC-FILE for parsers with LABEL."
  (let ((parser (cdr (assoc label wisi-process--alist))))
    (when parser
      (wisi-parse-kill parser)
      (setf (wisi-process--parser-exec-file parser) exec-file))))

(defun wisi-process-parse--require-process (parser)
  "Start the process for PARSER if not already started."
  (unless (process-live-p (wisi-process--parser-process parser))
    (let ((process-connection-type nil) ;; use a pipe, not a pty; avoid line-by-line reads
	  (process-name (format " *%s_wisi_parse*" (wisi-process--parser-label parser))))

      (unless (buffer-live-p (wisi-process--parser-buffer parser))
	;; User may have killed buffer to kill parser.
	(setf (wisi-process--parser-buffer parser)
	      (get-buffer-create process-name)))

      (with-current-buffer (wisi-process--parser-buffer parser)
	(erase-buffer)); delete any previous messages, prompt

      (setf (wisi-process--parser-process parser)
	    (if (fboundp 'make-process)
		;; emacs >= 25
		(make-process
		 :name process-name
		 :buffer (wisi-process--parser-buffer parser)
		 :command (append (list (wisi-process--parser-exec-file parser))
				  (wisi-process--parser-exec-opts parser)))
	      ;; emacs < 25
	      (start-process
	       process-name
	       (wisi-process--parser-buffer parser)
	       (wisi-process--parser-exec-file parser)
	       (wisi-process--parser-exec-opts parser)
	       )))

      (set-process-query-on-exit-flag (wisi-process--parser-process parser) nil)
      (setf (wisi-process--parser-busy parser) nil)

      ;; IMPROVEME: check protocol and version numbers
      (wisi-process-parse--wait parser)
      )))

(defun wisi-process-parse--wait (parser)
  "Wait for the current command to complete."
  (let ((process (wisi-process--parser-process parser))
	(search-start (point-min))
	(wait-count 0)
	(found nil))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (setq found (re-search-forward wisi-process-parse-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(setq wait-count (1+ wait-count))
	(when (> wisi-process-parse-debug 0)
	    (message "wisi-process-parse--wait: %d" wait-count))
	(accept-process-output process 0.1))

      (if found
	  (when (> wisi-process-parse-debug 0)
	    (message "wisi-process-parse--wait: %d" wait-count)
	    (when (> wisi-process-parse-debug 2)
	      (message "'%s'" (buffer-substring-no-properties (point-min) (point-max)))))

	(wisi-process-parse-show-buffer parser)
	(error "%s process died" (wisi-process--parser-exec-file parser)))
      )))

(defun wisi-process-parse-show-buffer (parser)
  "Show PARSER buffer."
  (if (buffer-live-p (wisi-process--parser-buffer parser))
      (pop-to-buffer (wisi-process--parser-buffer parser))
    (error "wisi-process-parse process not active")))

(defun wisi-process-parse--send-parse (parser line-count)
  "Send a parse command to PARSER external process, followed by
the content of the current buffer.  Does not wait for command to
complete."
  ;; Must match "parse" command arguments in gen_emacs_wisi_parse.adb
  (let* ((cmd (format "parse %d \"%s\" %d %d %d %d %d %d %d %d %d %d %d %s"
		      (cl-ecase wisi--parse-action
			(navigate 0)
			(face 1)
			(indent 2))
		      (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		      line-count
		      (if (> wisi-debug 0) 1 0) ;; debug-mode
		      (1- wisi-debug) ;; trace_parse
		      wisi-trace-mckenzie
		      wisi-trace-action
		      (if wisi-mckenzie-disable 1 0)
		      (if wisi-mckenzie-task-count wisi-mckenzie-task-count -1)
		      (if wisi-mckenzie-cost-limit wisi-mckenzie-cost-limit -1)
		      (if wisi-mckenzie-check-limit wisi-mckenzie-check-limit -1)
		      (if wisi-mckenzie-enqueue-limit wisi-mckenzie-enqueue-limit -1)
		      (1- (position-bytes (point-max)))
		      (wisi-parse-format-language-options parser)
		      ))
	 (msg (format "%03d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))
    (when (> wisi-process-parse-debug 0)
      (message msg))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (process-send-string process msg)
    (process-send-string process (buffer-substring-no-properties (point-min) (point-max)))

    ;; We don’t wait for the send to complete; the external process
    ;; may start parsing and send an error message.
    ))

(defun wisi-process-parse--send-noop (parser)
  "Send a noop command to PARSER external process, followed by
the content of the current buffer.  Does not wait for command to
complete."
  (let* ((cmd (format "noop %d" (1- (position-bytes (point-max)))))
	 (msg (format "%03d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))
    (when (> wisi-process-parse-debug 0)
      (message msg))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (process-send-string process msg)
    (process-send-string process (buffer-substring-no-properties (point-min) (point-max)))
    ))

(defun wisi-process-parse--marker-or-nil (item)
  (if (= -1 item) nil (copy-marker item t)))

(defun wisi-process-parse--Navigate_Cache (parser sexp)
  ;; sexp is [Navigate_Cache pos statement_id id length class containing_pos prev_pos next_pos end_pos]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1)))
    (with-silent-modifications
      (put-text-property
       pos
       (1+ pos)
       'wisi-cache
       (wisi-cache-create
	:nonterm    (aref (wisi-process--parser-token-table parser) (aref sexp 2))
	:token      (aref (wisi-process--parser-token-table parser) (aref sexp 3))
	:last       (aref sexp 4)
	:class      (aref wisi-class-list (aref sexp 5))
	:containing (wisi-process-parse--marker-or-nil (aref sexp 6))
	:prev       (wisi-process-parse--marker-or-nil (aref sexp 7))
	:next       (wisi-process-parse--marker-or-nil (aref sexp 8))
	:end        (wisi-process-parse--marker-or-nil (aref sexp 9))
	)))
    ))

(defun wisi-process-parse--Face_Property (parser sexp)
  ;; sexp is [Face_Property first-pos last-pos face-index]
  ;; see ‘wisi-process-parse--execute’
  ;; implements wisi--face-action-1
  (with-silent-modifications
    (add-text-properties
     (aref sexp 1)
     (1+ (aref sexp 2))
     (list 'font-lock-face (aref (wisi-process--parser-face-table parser) (aref sexp 3))
	   'fontified t)
     )))

(defun wisi-process-parse--Indent (parser sexp)
  ;; sexp is [Indent line-number indent]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref (wisi-process--parser-line-begin parser) (1- (aref sexp 1)))))
    (with-silent-modifications
      (put-text-property
       (1- pos)
       pos
       'wisi-indent
       (aref sexp 2)))
    ))

(defun wisi-process-parse--Lexer_Error (parser sexp)
  ;; sexp is [Lexer_Error char-position <message> <repair-char>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1))
	err)

    (goto-char pos);; for current-column

    (setq err
	  (make-wisi--lexer-error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: %s"
		   (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		   ;; file-name can be nil during vc-resolve-conflict
		   (line-number-at-pos pos)
		   (current-column)
		   (aref sexp 2))
	   :inserted (when (= 4 (length sexp)) (aref sexp 3))))

    (push err (wisi-parser-lexer-errors parser))
    ))

(defun wisi-process-parse--Parser_Error (parser sexp)
  ;; sexp is [Parser_Error char-position <string>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1))
	err)

    (goto-char pos);; for current-column

    (setq err
	  (make-wisi--parse-error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: %s"
		   (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		   ;; file-name can be nil during vc-resolve-conflict
		   (line-number-at-pos pos)
		   (1+ (current-column))
		   (aref sexp 2))))

    (push err (wisi-parser-parse-errors parser))
    ))

(defun wisi-process-parse--Check_Error (parser sexp)
  ;; sexp is [Check_Error code name-1-pos name-2-pos <string>]
  ;; see ‘wisi-process-parse--execute’
  (let* ((name-1-pos (aref sexp 2))
	(name-1-col (1+ (progn (goto-char name-1-pos)(current-column)))) ;; gnat columns are 1 + emacs columns
	(name-2-pos (aref sexp 3))
	(name-2-col (1+ (progn (goto-char name-2-pos)(current-column))))
	(file-name (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) ""))
	;; file-name can be nil during vc-resolve-conflict
	(err (make-wisi--parse-error
	      :pos (copy-marker name-1-pos)
	      :message
	      (format "%s:%d:%d: %s %s:%d:%d"
		      file-name (line-number-at-pos name-1-pos) name-1-col
		      (aref sexp 4)
		      file-name (line-number-at-pos name-2-pos) name-2-col)))
	)

    (push err (wisi-parser-parse-errors parser))
    ))

(defun wisi-process-parse--Recover (parser sexp)
  ;; sexp is [Recover [pos [inserted] [deleted]]...]
  ;; see ‘wisi-process-parse--execute’
  ;; convert to list of wisi--parse-error-repair, add to last error
  (let* ((token-table (wisi-process--parser-token-table parser))
	 (last-error (car (wisi-parser-parse-errors parser))))
    (unless (= 1 (length sexp))
      (cl-do ((i 1 (1+ i))) ((= i (length sexp)))
	(push
	 (make-wisi--parse-error-repair
	  :pos (aref (aref sexp i) 0)
	  :inserted (mapcar (lambda (id) (aref token-table id)) (aref (aref sexp i) 1))
	  :deleted  (mapcar (lambda (id) (aref token-table id)) (aref (aref sexp i) 2)))
	 (wisi--parse-error-repair last-error)))
      )))

(defun wisi-process-parse--execute (parser sexp)
  "Execute encoded SEXP sent from external process."
  ;; sexp is [action arg ...]; an encoded instruction that we need to execute
  ;;
  ;; Actions:
  ;;
  ;; [Navigate_Cache pos statement_id id length class containing_pos prev_pos next_pos end_pos]
  ;;    Set a wisi-cache text-property.
  ;;    *pos          : integer buffer position; -1 if nil (not set)
  ;;    *id           : integer index into parser-token-table
  ;;    length        : integer character count
  ;;    class         : integer index into wisi-class-list
  ;;
  ;; [Face_Property first-pos last-pos face-index]
  ;;    Set a font-lock-face text-property
  ;;    face-index: integer index into parser-elisp-face-table
  ;;
  ;; [Indent line-number indent]
  ;;    Set an indent text property
  ;;
  ;; [Lexer_Error char-position <message> <repair-char>]
  ;;    The lexer detected an error at char-position.
  ;;
  ;;    If <repair-char> is not ASCII NUL, it was inserted immediately
  ;;    after char-position to fix the error.
  ;;
  ;; [Parser_Error char-position <message>]
  ;;    The parser detected a syntax error; save information for later
  ;;    reporting.
  ;;
  ;;    If error recovery is successful, there can be more than one
  ;;    error reported during a parse.
  ;;
  ;; [Check_Error code name-1-pos name-2-pos <string>]
  ;;    The parser detected a semantic check error; save information
  ;;    for later reporting.
  ;;
  ;;    If error recovery is successful, there can be more than one
  ;;    error reported during a parse.
  ;;
  ;; [Recover [pos [inserted] [deleted]]...]
  ;;    The parser finished a successful error recovery.
  ;;
  ;;    pos: Buffer position
  ;;
  ;;    inserted: Virtual tokens (terminal or non-terminal) inserted before pos.
  ;;
  ;;    deleted: Tokens deleted after pos.
  ;;
  ;;    Args are token ids; index into parser-token-table. Save the information
  ;;    for later use by ’wisi-repair-error’.
  ;;
  ;;
  ;; Numeric action codes are given in the case expression below

  (cl-ecase (aref sexp 0)
    (1  (wisi-process-parse--Navigate_Cache parser sexp))
    (2  (wisi-process-parse--Face_Property parser sexp))
    (3  (wisi-process-parse--Indent parser sexp))
    (4  (wisi-process-parse--Lexer_Error parser sexp))
    (5  (wisi-process-parse--Parser_Error parser sexp))
    (6  (wisi-process-parse--Check_Error parser sexp))
    (7  (wisi-process-parse--Recover parser sexp))
    ))

;;;;; main

(cl-defmethod wisi-parse-kill ((parser wisi-process--parser))
  (when (process-live-p (wisi-process--parser-process parser))
    (process-send-string (wisi-process--parser-process parser) wisi-process-parse-quit-cmd)
    (sit-for 1.0)
    (when (process-live-p (wisi-process--parser-process parser))
      (kill-process (wisi-process--parser-process parser)))
    )
  (setf (wisi-process--parser-busy parser) nil))

(defvar wisi--lexer nil) ;; wisi-elisp-lexer.el
(declare-function wisi-elisp-lexer-reset "wisi-elisp-lexer")

(cl-defmethod wisi-parse-current ((parser wisi-process--parser))
  "Run the external parser on the current buffer."
  (wisi-process-parse--require-process parser)

  ;; font-lock can trigger a face parse while navigate or indent parse
  ;; is active, due to ‘accept-process-output’ below. font-lock must
  ;; not hang (it is called from an idle timer), so don’t
  ;; wait. Signaling an error tells font-lock to try again later.
  ;;
  ;; If the parser is left busy due to some error, that is a bug. In
  ;; order to detect such bugs, and avoid weird errors from
  ;; wisi-indent-region, we signal an error here.
  (if (wisi-process--parser-busy parser)
      (progn
	(setf (wisi-parser-parse-errors parser)
	      (list
	       (make-wisi--parse-error
		:pos 0
		:message (format "%s:%d:%d: parser busy (try ’wisi-kill-parser’)"
				 (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "") 1 1))
	       ))
	(error "%s parse abandoned; parser busy" wisi--parse-action)
	)

    ;; It is not possible for a background elisp function (ie
    ;; font-lock) to interrupt this code between checking and setting
    ;; parser-busy; background elisp can only run when we call
    ;; accept-process-output below.
    (setf (wisi-process--parser-busy parser) t)

    (condition-case-unless-debug err
	(let* ((source-buffer (current-buffer))
	       (response-buffer (wisi-process--parser-buffer parser))
	       (process (wisi-process--parser-process parser))
	       (w32-pipe-read-delay 0) ;; fastest subprocess read
	       response
	       response-end
	       (response-count 0)
	       (sexp-start (point-min))
	       (wait-count 0)
	       (need-more nil) ;; point-max if need more, to check for new input
	       (done nil)
	       start-wait-time)

	  (setf (wisi-process--parser-total-wait-time parser) 0.0)

	  (setf (wisi-parser-lexer-errors parser) nil)
	  (setf (wisi-parser-parse-errors parser) nil)

	  (let ((line-count (1+ (count-lines (point-min) (point-max)))))
	    (setf (wisi-process--parser-line-begin parser) (wisi--set-line-begin line-count))
	    (wisi-process-parse--send-parse parser line-count)

	    ;; We reset the elisp lexer, because post-parse actions may use it.
	    (when wisi--lexer
	      (wisi-elisp-lexer-reset line-count wisi--lexer))
	    )

	  (set-buffer response-buffer)

	  ;; process responses until prompt received
	  (while (not done)

	    ;; process all complete responses currently in buffer
	    (while (and (not need-more)
			(not done))

	      (goto-char sexp-start)

	      (cond
	       ((eobp)
		(setq need-more (point-max)))

	       ((looking-at wisi-process-parse-prompt)
		(setq done t))

	       ((or (looking-at "\\[") ;; encoded action
		    (looking-at "(")) ;; error or other elisp expression to eval
		(condition-case nil
		    (setq response-end (scan-sexps (point) 1))
		  (error
		   ;; incomplete response
		   (setq need-more (point-max))
		   nil))

		(unless need-more
		  (setq response-count (1+ response-count))
		  (setq response (car (read-from-string (buffer-substring-no-properties (point) response-end))))
		  (goto-char response-end)
		  (forward-line 1)
		  (setq sexp-start (point))

		  (set-buffer source-buffer) ;; for put-text-property in actions
		  (if (listp response)
		      ;; error of some sort
		      (cond
		       ((equal '(parse_error) response)
			;; Parser detected a syntax error, and recovery failed, so signal it.
			(if (wisi-parser-parse-errors parser)
			    (signal 'wisi-parse-error
				    (wisi--parse-error-message (car (wisi-parser-parse-errors parser))))

			  ;; can have no errors when testing a new parser
			  (push
			   (make-wisi--parse-error :pos 0 :message "parser failed with no message")
			   (wisi-parser-parse-errors parser))
			  (signal 'wisi-parse-error "parser failed with no message")))

		       ((equal 'parse_error (car response))
			;; Parser detected some other error non-fatal error, so signal it.
			(signal 'wisi-parse-error (cdr response)))

		       ((and (eq 'error (car response))
			     (string-prefix-p "bad command:" (cadr response)))
			;; Parser dropped bytes, is treating buffer
			;; content bytes as commands. Kill the process
			;; to kill the pipes; there is no other way to
			;; flush them.
			(kill-process (wisi-process--parser-process parser))
			(signal 'wisi-parse-error "parser lost sync; killed"))

		       (t
			;; Some other error
			(condition-case-unless-debug err
			    (eval response)
			  (error
			   (push (make-wisi--parse-error :pos (point) :message (cadr err)) (wisi-parser-parse-errors parser))
			   (signal (car err) (cdr err)))))
		       )

		    ;; else encoded action
		    (condition-case-unless-debug err
			(wisi-process-parse--execute parser response)
		      (wisi-parse-error
		       (push (make-wisi--parse-error :pos (point) :message (cadr err)) (wisi-parser-parse-errors parser))
		       (signal (car err) (cdr err)))))

		  (set-buffer response-buffer)
		  ))

	       (t
		;; debug output
		(forward-line 1)
		(setq sexp-start (point)))
	       )
	      )

	    (unless done
	      ;; end of response buffer
	      (unless (process-live-p process)
		(wisi-process-parse-show-buffer parser)
		(error "wisi-process-parse process died"))

	      (setq wait-count (1+ wait-count))
	      (setq start-wait-time (float-time))

	      ;; If we specify no time-out here, we get messages about
	      ;; "blocking call with quit inhibited", when this is
	      ;; called by font-lock from the display engine.
	      ;;
	      ;; Specifying just-this-one t prevents C-q from
	      ;; interrupting this?
	      (accept-process-output
	       process
	       wisi-process-time-out
	       nil ;; milliseconds
	       nil)  ;; just-this-one

	      (setf (wisi-process--parser-total-wait-time parser)
		    (+ (wisi-process--parser-total-wait-time parser)
		       (- (float-time) start-wait-time)))

	      (when (and (= (point-max) need-more)
		       (> wait-count 5))
		(error "wisi-process-parse not getting more text (or bad syntax in process output)"))

	      (setq need-more nil))
	    );; while not done

	  ;; got command prompt
	  (unless (process-live-p process)
	    (wisi-process-parse-show-buffer parser)
	    (error "wisi-process-parse process died"))

	  (setf (wisi-process--parser-response-count parser) response-count)

	  (setf (wisi-process--parser-busy parser) nil)
	  (set-buffer source-buffer)
	  ;; If we get here, the parse succeeded (possibly with error
	  ;; recovery); move point to end of buffer as the elisp
	  ;; parser does.
	  (goto-char (point-max))
	  )

      (wisi-parse-error
       (setf (wisi-process--parser-busy parser) nil)
       (signal (car err) (cdr err)))

      (error
       (setf (wisi-process--parser-busy parser) nil)
       (signal (car err) (cdr err))
       ))))

(defvar wisi--parser nil) ;; wisi.el

(defun wisi-process-send-tokens-noop ()
  "Run lexer, send tokens to subprocess; otherwise no operation.
For use with ’wisi-time’."
  (wisi-process-parse--require-process wisi--parser)
  (if (wisi-process--parser-busy wisi--parser)
      (error "%s parser busy" wisi--parse-action)

    ;; not busy
    (let* ((source-buffer (current-buffer))
	   (action-buffer (wisi-process--parser-buffer wisi--parser))
	   (process (wisi-process--parser-process wisi--parser))
	   (sexp-start (point-min))
	   (need-more nil)
	   (done nil))

      (setf (wisi-process--parser-busy wisi--parser) t)
      (wisi-process-parse--send-noop wisi--parser)

      (set-buffer action-buffer)
      (while (and (process-live-p process)
		  (not done))
	(goto-char sexp-start)
	(cond
	 ((eobp)
	  (setq need-more t))

	 ((looking-at wisi-process-parse-prompt)
	  (setq done t))

	 (t
	  (forward-line 1)
	  (setq sexp-start (point)))
	 )

	(unless done
	  ;; end of response buffer
	  (unless (process-live-p process)
	    (wisi-process-parse-show-buffer wisi--parser)
	    (error "wisi-process-parse process died"))

	  (accept-process-output process 1.0 nil nil)
	  (setq need-more nil))
	)
      (set-buffer source-buffer)
      (setf (wisi-process--parser-busy wisi--parser) nil)
      )))

;;;;; debugging
(defun wisi-process-parse-ids-to-enum (token-table &rest int-ids)
  "Translate INT-IDS from process integer token ids to elisp enumeral ids.
Returns reversed sequence."
  (let ((enum-ids nil))
    (cl-dolist (i int-ids)
      (push (aref token-table i) enum-ids))
    enum-ids))

(provide 'wisi-process-parse)
