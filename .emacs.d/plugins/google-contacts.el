;;; google-contacts.el --- implementation -*-coding: utf-8 -*-

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

;;   The Google Contacts package aims at providing an implementation of an 
;; interface between BBDB and Google Gmail Contacts
;;
;;  Dependences:
;;  - bbdb
;;  - Python
;;  - Google GData (download in the google calendar website).
;;  

;;; Code:

(require 'bbdb)

(defconst google-contacts-version "0.0.3"
  "Version number of google-contacts.el.")

;; ======================================================================
;; Customizables
;; ======================================================================
(defgroup google-contacts nil
  "Google Contacts support."
  :prefix "google-contacts-"
  :group 'bbdb)

(defvar google-contacts-user           nil             "Google user")
(defvar google-contacts-password       nil             "Google password")

(defvar google-contacts-directory      "/tmp" "Contacts files directory (temporary directory)")
(defvar google-contacts-tmp-file       "gctt.tmp"      "Google Contacts intermediate file")
(defvar google-contacts-code-directory nil             "Python Scripts directory" )
(defvar google-contacts-auto-update    nil              "Download contacts after updates")

(defun google-contacts-sync ( )
  "Copy Contacts from Gmail to BBDB."
  (interactive)
  (if (equal google-contacts-password nil)
	  (setq google-contacts-password 
			(read-passwd (concat "Type the password for the Google Account "
								 google-contacts-user 
								 "@gmail.com : "))))  

  (message "Downloading Google Contacts...")

;  (iswitchb-buffer "*BBDB*")
;  (switch-to-buffer "*BBDB*")
;  (if (equal (current-buffer) "*BBDB*")
;	  (kill-buffer nil))
;  (switch-to-buffer ".bbdb")
;  (if (equal (current-buffer) ".bbdb")
;	  (kill-buffer nil))

  (kill-buffer "*BBDB*")
  (kill-buffer ".bbdb")

  (shell-command-to-string (concat "python " google-contacts-code-directory "/syncContacts.py "
				   "--user " google-contacts-user " "
				   "--pw " google-contacts-password ))  

  (message "Downloading Google Contacts...Done")
  )

;;;;
(defun google-contacts-insert ( name email )
  "Insert Contact to Google"
  (interactive)
  (if (equal google-contacts-password nil)
	  (setq google-contacts-password 
			(read-passwd (concat "Type the password for the Google Account "
								 google-contacts-user 
								 "@gmail.com : "))))  
    
  (setq name    (read-from-minibuffer "Name : " name))
  (setq emacs   (read-from-minibuffer "Email : " email))
  
  (if (y-or-n-p (concat "Insert other details about " name "? "))
      (let ((company (read-from-minibuffer "Company : "))
	    (title   (read-from-minibuffer "Title : "))
	    (department (read-from-minibuffer "Department : "))
	    (phone   (read-from-minibuffer "Phone : "))
	    (street  (read-from-minibuffer "Address - Street and Number : "))
	    (city    (read-from-minibuffer "Address - City : "))
	    (state   (read-from-minibuffer "Address - State : "))
	    (zip     (read-from-minibuffer "Address - Zip code : "))
	    (country (read-from-minibuffer "Address - Country : "))
	    (notes   (read-from-minibuffer "Notes/Comments : "))
	    )
	(shell-command-to-string (concat "python " google-contacts-code-directory "/insertContact.py "
;	(message (concat "python " google-contacts-code-directory "/insertContact.py "
				     "--user " google-contacts-user " "
				     "--pw "   google-contacts-password " "
				     "--name \"" name "\" "
				     "--email \"" email "\" "
				     "--company \"" company "\" "
				     "--title \"" title "\" "
				     "--department \"" department "\" "
				     "--phone \"" phone "\" "
				     "--street \"" street "\" "
				     "--city \"" city "\" "
				     "--state \"" state "\" "
				     "--zip \"" zip "\" "
				     "--notes \"" notes "\" "
				     "--country \"" country "\" ")
				 ))
    (shell-command-to-string (concat "python " google-contacts-code-directory "/insertContact.py "
;    (message (concat "python " google-contacts-code-directory "/insertContact.py "
				     "--user " google-contacts-user " "
				     "--pw "   google-contacts-password " "
				     "--name \"" name "\" "
				     "--email \"" email "\""))
    )

    (if (not (equal google-contacts-auto-update nil))
      (google-contacts-sync))

;  (message (concat "Google Contacts: Added " name " <" email "> "))
  )

;;;;;;;;;;;;;;
(defun google-contacts-bbdb-create ( reg )
  "Get te created register from BBDB"
  
  (setq name   (bbdb-record-name reg))
;  (setq emails (bbdb-record-net reg))
  (setq  emails  (bbdb-record-net reg))
;  (let (emails)
;	(dolist (email (bbdb-record-net reg))
;	  (setq emails (cons email emails))))

  (if (y-or-n-p (concat "Add " name " to Google Contacts ? "))
	  (google-contacts-insert name (car emails)))

  )


(add-hook 'bbdb-create-hook 'google-contacts-bbdb-create) 


(provide 'google-contacts)


;;; google-contacts.el ends here
