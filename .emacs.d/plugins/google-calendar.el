;;; google-calendar.el --- iCalendar implementation -*-coding: utf-8 -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.

;; Author:         Cristiano lazzari <crlazzari@gmail.com>
;; Created:        Nov 2009
;; Keywords:       google calendar
;; Human-Keywords: google, calendar, diary

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;   The Google Calendar package aims at providing an implementation of an 
;; interface between emacs  diary and Google Calendar
;;
;;  Dependences:
;;  - icalendar.el
;;  - Python
;;  - Google GData (download in the google calendar website).
;;  

;;; Code:

(require 'icalendar)

(defconst google-calendar-version "0.0.3"
  "Version number of google-calendar.el.")

;; ======================================================================
;; Customizables
;; ======================================================================
(defgroup google-calendar nil
  "Google Calendar support."
  :prefix "google-calendar-"
  :group 'calendar)

;;;;;;;;;;;;;;;;;;;
;; Variables
(defvar google-calendar-user           nil "Google calendar user")
(defvar google-calendar-password       nil "Google calendar password")
(defvar google-calendar-directory      "/tmp" "Calendar files directory (temporary directory)")
(defvar google-calendar-url            nil "Google calendar URL")
(defvar google-calendar-code-directory nil "Python code")
(defvar google-calendar-auto-update    nil "Download calendar after updates")


;;;;;;;;;;;;;;;;;;;;
;; Function

; Download Google Calendar
(defun google-calendar-download ()
  "Download diary from Google and replace local diary using shell commands."  
  (interactive)
  (setq bfname (buffer-name))
  (shell-command-to-string (concat "mv -f " diary-file  " " diary-file ".backup" ))
  (shell-command-to-string (concat "cd " google-calendar-directory "; "
				   "rm -f basic.ics; rm -f diary;"
				   "wget " google-calendar-url "; "))
  (icalendar-import-file (concat  google-calendar-directory "/basic.ics" )
			 (concat  google-calendar-directory "/diary" ))
  (switch-to-buffer (find-file-noselect (concat  google-calendar-directory "/basic.ics" )))
  (kill-buffer nil)
  (switch-to-buffer (find-file-noselect (concat  google-calendar-directory "/diary" )))
  (kill-buffer nil)
  (shell-command-to-string (concat "cp -f " google-calendar-directory "/diary " diary-file))
;  (revert-buffer diary-file)
;  (revert-buffer t t)
  (switch-to-buffer bfname)  
  (message "Diary updated from Google Calendar.")
  )


; Quick add in the Google Calendar
(defun google-calendar-quick-add-event ()
  "Google Calendar Add Quick Event. (e.g Dinner with Angelina Jolie 11/21 at 21pm)"  
  (interactive)
  (if (equal google-calendar-password nil)
      (setq google-calendar-password 
	    (read-passwd (concat "Type the password for the Google Account "
				 google-calendar-user 
				 "@gmail.com : "))))
  (setq google-calendar-quick-add-event (read-from-minibuffer "Add Quick Event : "))
  (shell-command-to-string (concat "python " google-calendar-code-directory "/insertQuickEvent.py "
				   "--user " google-calendar-user " "
				   "--pw " google-calendar-password " "
				   "--msg \"" google-calendar-quick-add-event "\""))
  (if (not (equal google-calendar-auto-update nil))
      (google-calendar-download))
)

; Add in the Google Calendar
(defun google-calendar-add-event ()
  "Google Calendar add event."
  (interactive)
  (if (equal google-calendar-password nil)
      (setq google-calendar-password 
	    (read-passwd (concat "Type the password for the Google Account "
				 google-calendar-user 
				 "@gmail.com : "))))
  (setq google-calendar-title (read-from-minibuffer "What : "))
  (setq google-calendar-where (read-from-minibuffer "Where : "))
  (if (equal (length google-calendar-where) 0)
      (setq google-calendar-where "-"))
  (setq google-calendar-content (read-from-minibuffer "Description : "))
  (if (equal (length google-calendar-content) 0)
      (setq google-calendar-content "-"))
  ;; All day
  (if (y-or-n-p "All day event : ")
      (setq google-calendar-allday "Y")
    (setq google-calendar-allday "N"))

  (setq year (format-time-string "%Y"))
  (setq month (format-time-string "%m"))
  (setq day (format-time-string "%d"))
  ;; Start DATE TIME
  (setq google-calendar-start-day (read-from-minibuffer "Start (day) [[[YYYY]MM]DD] (empty for today): "))
  (if (equal (length google-calendar-start-day) 0 )
      (setq google-calendar-start-day (concat year month day) )
    (if (equal (length google-calendar-start-day) 2 )
	(setq google-calendar-start-day (concat year month google-calendar-start-day) )
      (if (equal (length google-calendar-start-day) 4 )
	(setq google-calendar-start-day (concat year google-calendar-start-day) ) ) ) )
  (setq google-calendar-start google-calendar-start-day)
  (if (equal google-calendar-allday "N")
;      ( (setq google-calendar-start-time (read-from-minibuffer "Start (hour) HHMM : "))
	(setq google-calendar-start (concat google-calendar-start-day "T"
					   (read-from-minibuffer "Start (hour) HHMM : ") "00Z" )))
  ;; End DATE TIME
  (setq google-calendar-end-day (read-from-minibuffer "End (day) [[[YYYY]MM]DD] : " google-calendar-start-day ))
  (if (equal (length google-calendar-end-day) 0 )
      (setq google-calendar-end-day google-calendar-start-day  )
    (if (equal (length google-calendar-end-day) 2 )
	(setq google-calendar-end-day (concat year month google-calendar-end-day) )
      (if (equal (length google-calendar-end-day) 4 )
	  (setq google-calendar-end-day (concat year google-calendar-end-day) ) ) ) )
  (setq google-calendar-end google-calendar-end-day)
  (if (equal google-calendar-allday "N")
;      ((setq google-calendar-end-time (read-from-minibuffer "End (hour) HHMM : "))
       (setq google-calendar-end (concat google-calendar-end-day "T"
					 (read-from-minibuffer "End (hour) HHMM : ") "00Z")))       
  
  ;; Recurring
  (if (y-or-n-p "Recurring : ")
      (setq google-calendar-recurr "Y")
    (setq google-calendar-recurr "N"))

  (setq google-calendar-rrule "")

  (if (equal google-calendar-recurr "Y")
      (setq google-calendar-freq-q (read-from-minibuffer "Frequency (D=DAYLY,W=WEEKLY,M=MONTHLY,Y=YEARLY) : ")))

  (if (equal google-calendar-recurr "Y")
      (if (or (equal google-calendar-freq-q "Y") (equal google-calendar-freq-q "y"))
	  (setq google-calendar-rrule "FREQ=YEARLY;WKST=SU")
	(if (or (equal google-calendar-freq-q "M") (equal google-calendar-freq-q "m"))
	    (setq google-calendar-rrule (concat "FREQ=MONTHLY;INTERVAL=" 
						(read-from-minibuffer "Interval (1 means every month) : " "1" ) ";"
						"BYMONTHDAY="
						(substring google-calendar-start-day 6 8) ";"				  
						)
	       )
	    (if (or (equal google-calendar-freq-q "W") (equal google-calendar-freq-q "w"))
		(setq google-calendar-rrule (concat "FREQ=WEEKLY;INTERVAL=" 
						    (read-from-minibuffer "Interval (1 means every week) : " "1" ) ";"
						    "BYDAY=" 
						    (read-from-minibuffer "Days SU,MO,TU,WE,TH,FR,SA sepperated by comma: "))
		 )
	      (	      
	       (setq google-calendar-rrule (concat "FREQ=DAYLY;INTERVAL=" 
						   (read-from-minibuffer "Interval (1 means every day) : " "1")
						     )
		     ))))))
	

;;   (message "%s-%s-%s-%s-%s-%s-%s-%s" 
;; 	   google-calendar-title 
;; 	   google-calendar-where
;; 	   google-calendar-content
;; 	   google-calendar-allday
;; 	   google-calendar-start
;; 	   google-calendar-end
;; 	   google-calendar-recurr
;; 	   google-calendar-rrule
;; 	   )
  
  (shell-command-to-string (concat "python " google-calendar-code-directory "/insertEvent.py "
				   "--user " google-calendar-user " "
				   "--pw " google-calendar-password " "
				   "--t \"" google-calendar-title "\" "
				   "--c \"" google-calendar-content "\" "
				   "--w \"" google-calendar-where "\" "
				   "--st " google-calendar-start " "
				   "--et " google-calendar-end " "
				   "--r " google-calendar-recurr " "
				   "--rr \"" google-calendar-rrule "\" "
				   "--ad " google-calendar-allday ))
  
  (if (not (equal google-calendar-auto-update nil))
      (google-calendar-download))

  )

; Delete rvent in the Google Calendar
(defun google-calendar-delete-event ()
  "Google Calendar delete event."
  (interactive)

  (if (equal google-calendar-password nil)
      (setq google-calendar-password 
	    (read-passwd (concat "Type the password for the Google Account "
				 google-calendar-user 
				 "@gmail.com : "))))
  
  (beginning-of-line)(setq begpos (point))
  (search-forward "UID:")(setq uidpos (point))
  (search-forward "@")(setq endpos (point))
;  (goto-char uidpos)
;  (search-forward ")")(setq endpos (point))
;  (if (> endpos endpos2)
;	  (setq endpos endpos2))
;  (end-of-line)(setq endpos (point))
  (setq uid (buffer-substring (+ uidpos 1) (- endpos 1)))
  (if (> uidpos begpos)
      (if (y-or-n-p (concat "Ok to delete event \"" uid "\" ? "))
	  (shell-command-to-string (concat "python " google-calendar-code-directory "/deleteEvent.py "
				   "--user " google-calendar-user " "
				   "--pw " google-calendar-password " "
				   "--event \"" uid "\"" ))
	(message "Event was not deleted!!")
	))  
  (if (not (equal google-calendar-auto-update nil))
      (google-calendar-download)) 
  )

;;;;;;;
;; Key defs
(define-key calendar-mode-map "\C-c\C-gd" 'google-calendar-download)
(define-key calendar-mode-map "\C-c\C-gq" 'google-calendar-quick-add-event)
(define-key calendar-mode-map "\C-c\C-ga" 'google-calendar-add-event)

;(define-key diary-mode-map "\C-c\C-gd" 'google-calendar-download)
;(define-key diary-mode-map "\C-c\C-gq" 'google-calendar-quick-add-event)
;(define-key diary-mode-map "\C-c\C-ga" 'google-calendar-add-event)
;(define-key diary-mode-map "\C-c\C-ge" 'google-calendar-delete-event)

(provide 'google-calendar)


;;; google-calendar.el ends here
