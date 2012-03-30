;;; misc.el --- a config file for misc settings

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, misc
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-misc'
;;              var     `section-environment-os-recognition'

;;; Change Log:
;; 2012-03-29 (1.3)
;;    translate comments in english + calendar + diary
;; 2011-07-25 (1.2)
;;    add test about ms windows for cygwin shell
;; 2011-04-21 (1.1)
;;    add user name + longitude latitude + calendar
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; remove useless space at the end of line
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; define user name
(setq user-full-name "Claude TETE")

;; use compression
(auto-compression-mode t)

;; be sure that a new line is at the end of a file when it's saved
(setq require-final-newline t)
;;
;; to do not have bug `backup-directory-alist' for TRAMP files
(add-to-list 'backup-directory-alist
  (cons tramp-file-name-regexp nil))

;; fill-xxx is set with a width of 78 character
(custom-set-variables
  '(fill-column 78))

;; Set web browser to use
;; I don't use it
(setq browse-url-generic-program "opera")
(setq browse-url-browser-function 'browse-url-w3)

;;
;;; CALENDAR
;; REQUIREMENT: var     `clt-working-environment'
(when section-misc-calendar (message "  11.1 Calendar...")
  ;; start week with monday in 'calendar'
  (defvar calendar-week-start-day 1)
  ;;
  ;; to have Sunrise/Sunse time
  (cond
    ;; Magneti Marelli ---------------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (setq calendar-latitude 46.84)
      (setq calendar-longitude 0.55)
      (setq calendar-location-name "Chatellerault, FR")
      ) ; Magneti Marelli

    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (setq calendar-latitude 45.76)
      (setq calendar-longitude 4.91)
      (setq calendar-location-name "Villeurbanne, FR")
      ) ; Alstom Transport

    ) ; cond -------------------------------------------------------------------

  ;; set holidays
  (setq calendar-general-holidays nil)
  (setq calendar-christian-holidays nil)
  (setq calendar-hebrew-holidays nil)
  (setq calendar-islamic-holidays nil)
  (setq calendar-oriental-holidays nil)
  (setq calendar-solar-holidays nil)

  ;; ??
  (setq european-calendar-style 't)

  ;; display today mark in calendar
  (setq today-visible-calendar-hook 'calendar-mark-today)
  (setq calendar-today-marker 'highlight)

  ;; load french holidays
  (when (try-require 'french-holidays)
    (setq calendar-holidays holiday-french-holidays))


  (custom-set-variables
    ;; set display date in european format
    '(calendar-date-display-form
       (quote ((if dayname
                 (concat dayname " ")) day " " monthname " " year)))
    ;; set name of day's week
    '(calendar-day-name-array
       [
         "Dimanche"
         "Lundi"
         "Mardi"
         "Mercredi"
         "Jeudi"
         "Vendredi"
         "Samedi"])
    ;;;; holidays are visible in calendar
    ;;'(calendar-mark-holidays-flag t)
    ;; set name of month's year
    '(calendar-month-name-array
       [
         "Janvier"
         "Fevrier"
         "Mars"
         "Avril"
         "Mai"
         "Juin"
         "Juillet"
         "Aout"
         "Septembre"
         "Octobre"
         "Novembre"
         "Decembre"
         ])
    ;; set display time in 24H format
    '(calendar-time-display-form
             (quote (24-hours ":" minutes
                      (if time-zone " (") time-zone (if time-zone ")"))))
    ;; do not show holidays by default
    '(calendar-view-holidays-initially-flag nil)
    ;; show diary mark in calendar
    '(calendar-mark-diary-entries-flag t)
    ;; set name of lunar phase
    '(lunar-phase-names
       (quote ("Nouvelle Lune" "Premier Quartile" "Plein Lune" "Dernier Quartile")))
    )

  ;; TRY
  ;; Diary (by Marc Tommasi)
  (setq view-diary-entries-initially t
    diary-file (concat dotemacs-path "/diary")
    mark-diary-entries-in-calendar t
    number-of-diary-entries 7)

  (add-hook 'diary-display-hook 'fancy-diary-display)
  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

  (defun start-diary ()
    "Starts the diary."
    (calendar)
    (setq number-of-diary-entries 5)
                                        ;(view-diary-entries)
    (diary)
    (mark-diary-entries)
    (other-window 1))


  ;; Montrer les rendez-vous
  (add-hook 'diary-hook 'appt-make-list)
  (display-time)
  (add-hook 'diary-hook 'appt-make-list)
  (diary 0)

  (start-diary)

  (message "  11.1 Calendar... Done"))

;;
;; set cygwin shell
;; on MS Windows it works bad
(when running-on-ms-windows
  (cond
    ;; Magneti Marelli ---------------------------------------------------------
    ((string= clt-working-environment "Magneti Marelli")
      (setq w32shell-cygwin-bin "d:/cygwin/bin/zsh.exe")
      ) ; Magneti Marelli

    ;; Alstom Transport --------------------------------------------------------
    ((string= clt-working-environment "Alstom Transport")
      (setq w32shell-cygwin-bin "d:/cygwin/bin/zsh.exe")
      ) ; Alstom Transport

    ) ; cond -------------------------------------------------------------------
  )

;;
;;;; dictionnary lanaguage
;;;; never used
;;(setq ispell-dictionary "english")

;;; misc.el ends here
