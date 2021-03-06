;; Made by Julien Mulot <mulot@epita.fr>
;; EpX Member
;; Program under GPL
;;


(defconst max-line 25 "max number of line for a function")
(defconst max-func 5 "max number of function in a file")
(defconst max-line-len 79 "max lenght of line")


(defvar iserror 0 "count error that occured")

(defun detect-comment ()
  (progn
    (if (looking-at "^\\/\\*")
	(progn
	  (while (not (looking-at "\\*\\/"))
	    (forward-char 1))
	  (forward-char 2)
	  t)
      nil)
    )
  )

(defun detect-char ()
  (progn
    (if (looking-at "'")
	(progn
	  (forward-char -1)
	  (if (not (looking-at "\\\\"))
	      (progn
		(forward-char 2)
		(while (not (looking-at "'"))
		  (forward-char 1))
		(forward-char 1)))
	  )))
  nil)


(defun detect-string ()
  (progn
    (if (looking-at "\"")
	(progn
	  (forward-char -1)
	  (if (not (looking-at "\\\\"))
	      (progn
		(forward-char 2)
		(while (not (looking-at "\""))
		  (forward-char 1))
		(forward-char 1)))
	  )))
  nil)


(defun check-line ()
  (let ((nbfunc 0))
    (goto-char (point-min))
    (while (< (point) (point-max))      
      (detect-comment)
      (forward-line 1)
      (if (looking-at "^{")
	  (let ((nbline 0))
	    (setq nbfunc (1+ nbfunc))
	    (forward-line -1)
	    (setq funcname (point))
	    (forward-line 1)
	    (forward-line 1)
	    (while (not (looking-at "^}"))
	      (setq nbline (1+ nbline))
	      (forward-line 1))
	    (if (> nbline max-line)
		(let ()
		  (setq iserror (+ 1 iserror))
		  (setq here (point))
		  (goto-char funcname)
		  (message "function of %d lines" nbline)
		  (read-char)
		  (goto-char here)))))
      (if (> nbfunc max-func)
	  (let ()
	    (setq iserror (+ 1 iserror))
	    (message "%d functions in this file" nbfunc)))))
  nil)


(defun check-line-len ()
  (progn
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (detect-comment)
	  (setq line-len (- (point) (line-beginning-position))))
      (detect-string)
      (detect-char)
      (if (= (line-beginning-position) (point))
	  (setq line-len 1))
      (if (not (looking-at "^#"))
	  (progn
	    (if (looking-at ";")
		(progn
		  (forward-char -1)
		  (if (looking-at " ")
		      (progn
			(setq iserror (+ 1 iserror))
			(message "no space before ;")
			(read-char)))
		  (forward-char +1)))
	    (if (or (looking-at "+")
		    (looking-at "-")
		    ;;		    (looking-at "*")
		    (looking-at "/")
		    (looking-at "=")
		    (looking-at "&")
		    (looking-at ">")
		    (looking-at "<")
		    (looking-at "|"))
		(progn
		  (setq here (point))
		  (setq ispace 0)
		  (setq ispointer 0)
		  (setq isaddress 0)
		  (setq iscrementation 0)
		  (setq isnegatif 0)
		  (if (looking-at "-")
		      (progn
			(forward-char 1)
			(if (looking-at ">")
			    (progn
			      (setq ispointer 1)
			      (setq line-len (1+ line-len)))
			  (progn
			    (if (looking-at "-")
				(setq iscrementation 1)))
			  (progn
			    (if (or (looking-at "[0-9]")
				    (looking-at "[a-zA-Z]"))
				(progn
				  (forward-char -3)
				  (if (or (looking-at "+")
					  (looking-at "-")
					  (looking-at "/")
					  (looking-at "=")
					  (looking-at "&")
					  (looking-at ">")
					  (looking-at "<")
					  (looking-at "|"))
				      (setq isnegatif 1)))))
			  (goto-char here))))
		  (if (looking-at "+")
		      (progn
			(forward-char 1)
			(if (looking-at "+")
			    (setq iscrementation 1)
			  (goto-char here))))
		  (if (looking-at "&")
		      (progn
			(forward-char 1)
			(if (or (looking-at "(")
				(looking-at "[a-z]")
				(looking-at "[A-Z]"))
			    (setq isaddress 1))
			(goto-char here)))
		  (if (and (= ispointer 0)
			   (= iscrementation 0)
			   (= isaddress 0)
			   (= isnegatif 0))
		      (progn
			(forward-char -1)
			(setq prev-char (char-to-string (char-after (point))))
			(while (looking-at " ")
			  (setq ispace (1+ ispace))
			  (forward-char -1))
			(if (and (> ispace 0)
				 (equal (char-to-string (char-after here)) "=")
				 (or (equal (char-to-string (char-after (point))) "+")
				     (equal (char-to-string (char-after (point))) "-")
				     (equal (char-to-string (char-after (point))) "%")
				     (equal (char-to-string (char-after (point))) "*")
				     (equal (char-to-string (char-after (point))) "/")
				     (equal (char-to-string (char-after (point))) "=")
				     (equal (char-to-string (char-after (point))) "!")
				     (equal (char-to-string (char-after (point))) "&")
				     (equal (char-to-string (char-after (point))) "|")
				     (equal (char-to-string (char-after (point))) "<")
				     (equal (char-to-string (char-after (point))) ">")))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "no space between %c and =" (char-after (point)))
			      (read-char)))
			(if (and (> ispace 0)
				 (equal (char-to-string (char-after here)) ">")
				 (equal (char-to-string (char-after (point))) ">"))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "no space between > and >")
			      (read-char)))
			(if (and (> ispace 0)
				 (equal (char-to-string (char-after here)) "<")
				 (equal (char-to-string (char-after (point))) "<"))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "no space between < and <")
			      (read-char)))
			(if (and (> ispace 0)
				 (equal (char-to-string (char-after here)) "|")
				 (equal (char-to-string (char-after (point))) "|"))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "no space between | and |")
			      (read-char)))
			(if (and (> ispace 0)
				 (equal (char-to-string (char-after here)) "&")
				 (equal (char-to-string (char-after (point))) "&"))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "no space between & and &")
			      (read-char)))
			(if (> ispace 1)
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "too many spaces for %c" (char-after here))
			      (read-char)))
			(if (and (= ispace 0)
				 (not (or (equal prev-char "+")
					  (equal prev-char "-")
					  (equal prev-char "%")
					  (equal prev-char "*")
					  (equal prev-char "/")
					  (equal prev-char "=")
					  (equal prev-char "!")
					  (equal prev-char "&")
					  (equal prev-char "|")
					  (equal prev-char "<")
					  (equal prev-char ">"))))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "%c need 1 space A" (char-after here))
			      (read-char)))
			(goto-char here)
			(setq ispace 0)
			(forward-char 1)
			(setq next-char (char-to-string (char-after (point))))
			(while (looking-at " ")
			  (setq ispace (1+ ispace))
			  (forward-char 1))
			(if (> ispace 1)
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "too many spaces for %c" (char-after here))
			      (read-char)))
			(if (and (= ispace 0)
				 (not (or (equal next-char "=")
					  (equal next-char "|")
					  (equal next-char ">")
					  (equal next-char "<")
					  (equal next-char "-")
					  (equal next-char "+")
					  (equal next-char "&"))))
			    (progn
			      (setq iserror (+ 1 iserror))
			      (message "%c need 1 space B" (char-after here))
			      (read-char)))
			(goto-char here)))))
	    (if (> line-len (+ 1 max-line-len))
		(progn
		  (setq iserror (+ 1 iserror))
		  (message "line too long : %d characters" (- line-len 1))
		  (read-char)))
	    (forward-char 1)
	    (setq line-len (1+ line-len)))
	(forward-line 1))))
  nil)



(defun check-space-func-and-keyword ()
  (progn
    (setq beginline 0)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (and (> (point) (line-beginning-position))
	       (/= beginline (line-beginning-position)))
	  (progn
	    (goto-char beginline)
	    (forward-line 1)))
      (if (= (line-beginning-position) (point))
	  (setq beginline (point)))
      (setq ispace 0)
      (setq prevword (current-word))
      (while (and (looking-at " ")
		  (/= (point) (line-beginning-position)))
	(setq ispace (1+ ispace))
	(forward-char 1)
	(if (= (point) (line-beginning-position))
	    (setq ispace 0)))
      (if (detect-comment)
	  (progn
	    (setq beginline (point))))
      (if (looking-at "(")
	  (progn
	    (if (or (equal prevword "if")
		    (equal prevword "while")
		    (equal prevword "for")
		    (equal prevword "return")
		    (equal prevword "sizeof"))
		(progn
		  (if (= ispace 0)
		      (progn
			(setq iserror (+ 1 iserror))
			(message "%s need 1 space" prevword)
			(read-char)))
		  (if (> ispace 1)
		      (progn
			(setq iserror (+ 1 iserror))
			(message "too many spaces for %s" prevword)
			(read-char))))
	      (if (> ispace 0)
		  (progn
		    (setq iserror (+ 1 iserror))
		    (message "too many spaces for %s" prevword)
		    (read-char))))))
      (forward-word 1)))
  nil)

(defun norme ()
  (interactive)
  (setq iserror 0)
  (save-excursion
    (setq iserror 0)
    (check-space-func-and-keyword)
    (check-line)
    (check-line-len)
    (if (= iserror 0)
	(message "Everything is OK")
      (message "%d errors found in this file..." iserror)))
  (setq iserror 0))
