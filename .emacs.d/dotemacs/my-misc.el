;;; my-misc.el --- a config file for misc settings

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
;; Version: 2.1
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-misc'
;;              var     `section-environment-os-recognition'

;;; Change Log:
;; 2012-06-26 (2.1)
;;    no scratch message + size of tab + backspace on tab remove one space
;; 2012-06-21 (2.0)
;;    add generic browser
;; 2012-06-15 (1.9)
;;    add zone mode (screensaver)
;; 2012-06-12 (1.8)
;;    fix indentation in start message
;; 2012-06-08 (1.7)
;;    fix bug about fill column + warning about undo too long
;; 2012-05-04 (1.6)
;;    remove try about diary, add ignore case during completion, set path of
;;    bookmarks + add Dired human size
;; 2012-04-25 (1.5)
;;    remove setting to remove tooltip
;; 2012-04-19 (1.4)
;;    add dictionary + keep Minibuffer history
;; 2012-03-29 (1.3)
;;    translate comments in English + calendar + diary
;; 2011-07-25 (1.2)
;;    add test about ms windows for Cygwin shell
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
;;FIXME working environment default
(setq user-full-name profile-username)

;; use compression
(auto-compression-mode t)

;; be sure that a new line is at the end of a file when it's saved
(setq require-final-newline t)
;;
;; to do not have bug `backup-directory-alist' for TRAMP files
(add-to-list 'backup-directory-alist
  (cons tramp-file-name-regexp nil))

;; fill-xxx is set with a width of profile-fill-column
(custom-set-variables
  '(fill-column profile-fill-column))

;; keep history between session of emacs (even after close it) for Minibuffer
(savehist-mode 1)

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)

;; check all variables and non-interactive functions with apropos
(setq apropos-do-all t)

;; change size display in Dired mode
(setq dired-listing-switches "-alh")

;; remove warning about undo too long
(custom-set-variables
  '(warning-suppress-types (quote ((undo discard-info))))
)

;; set browser to open url
(setq browse-url-generic-program profile-browser)
(setq browse-url-browser-function 'browse-url-generic)

;; don't insert instructions into the *scratch* buffer
(setq initial-scratch-message nil)

;; tab = x spaces
(setq-default default-tab-width 8)
(setq-default tab-width 8)

;; set tab stops based on default-tab-width
(setq-default tab-stop-list (loop for i
                                  from default-tab-width to 120
                                  by default-tab-width
                                  collect i))

;; backspace on whitespace turns to spaces and removes one
(setq backward-delete-char-untabify-method 'untabify)

;;;; Annoying arrows mode: ring bell when toooo much same arrow move and suggest
;;;; new command to move
;;(require 'annoying-arrows-mode)
;;(global-annoying-arrows-mode)

;;
;;; CALENDAR
(when section-misc-calendar (message "  11.1 Calendar...")
  ;; start week with Monday in 'calendar'
  (defvar calendar-week-start-day 1)
  ;;
  ;; to have Sunrise/Sunset time
  (setq calendar-latitude profile-latitude)
  (setq calendar-longitude profile-longitude)
  (setq calendar-location-name profile-location-name)

  (custom-set-variables
    ;; do not show holidays by default
    '(calendar-view-holidays-initially-flag nil)
    ;; show diary mark in calendar
    '(calendar-mark-diary-entries-flag t)
    )

  (when section-misc-calendar-french (message "    11.1.1 French calendar...")
    ;; display today mark in calendar
    (setq today-visible-calendar-hook 'calendar-mark-today)
    (setq calendar-today-marker 'highlight)

    ;; set holidays
    (setq calendar-general-holidays nil)
    (setq calendar-christian-holidays nil)
    (setq calendar-hebrew-holidays nil)
    (setq calendar-islamic-holidays nil)
    (setq calendar-oriental-holidays nil)
    (setq calendar-solar-holidays nil)

    ;; ??
    (setq european-calendar-style 't)

    ;; load French holidays
    (when (try-require 'french-holidays "      ")
      (setq calendar-holidays holiday-french-holidays))


    (custom-set-variables
      ;; set display date in European format
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
      ;; set name of lunar phase
      '(lunar-phase-names
         (quote ("Nouvelle Lune" "Premier Quartile" "Plein Lune" "Dernier Quartile")))
      )
    (message "    11.1.1 French calendar...done")
    ) ; (when section-misc-calendar-french

  ;; TRY
  ;; Diary (by Marc Tommasi)
;;  (setq view-diary-entries-initially t
;;    diary-file (concat dotemacs-path "/diary")
;;    mark-diary-entries-in-calendar t
;;    number-of-diary-entries 7)
;;
;;  (add-hook 'diary-display-hook 'fancy-diary-display)
;;  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;;
;;  (defun start-diary ()
;;    "Starts the diary."
;;    (calendar)
;;    (setq number-of-diary-entries 5)
;;                                        ;(view-diary-entries)
;;    (diary)
;;    (mark-diary-entries)
;;    (other-window 1))
;;
;;
;;  ;; show rendez-vous
;;  (add-hook 'diary-hook 'appt-make-list)
;;  (display-time)
;;  (add-hook 'diary-hook 'appt-make-list)
;;  (diary 0)
;;
;;  (start-diary)
;  (diary)

  (message "  11.1 Calendar... Done"))

;;
;; set Cygwin shell
;; on MS Windows it works bad
(when running-on-ms-windows
  (setq w32shell-cygwin-bin profile-shell-cygwin))

;;
;;;; DICTIONARY language
(when section-misc-dictionary (message "  11.2 Dictionary...")
  (setq ispell-program-name profile-ispell-program)
  (setq ispell-dictionary profile-ispell-dictionary)
  (try-require 'ispell "    ")
  (message "  11.2 Dictionary... Done"))

;;
;;;; BOOKMARK
(when section-misc-bookmark (message "  11.3 Bookmarks...")
  ;; where to save the bookmarks
  (setq bookmark-default-file (concat dotemacs-path "/bookmarks"))
  ;;
  ;; each command that sets a bookmark will also save your bookmarks
  (setq bookmark-save-flag 1)
  (message "  11.3 Bookmark... Done"))

;;
;;;; SCREENSAVER
(when section-misc-screensaver (message "  11.4 Screensaver...")
  (setq zone-programs
    [
      zone-pgm-jitter
      zone-pgm-putz-with-case
      ;;zone-pgm-dissolve
      ;;zone-pgm-explode
      ;;zone-pgm-whack-chars
      zone-pgm-rotate
      zone-pgm-rotate-LR-lockstep
      zone-pgm-rotate-RL-lockstep
      zone-pgm-rotate-LR-variable
      zone-pgm-rotate-RL-variable
      zone-pgm-drip
      zone-pgm-drip-fretfully
      zone-pgm-five-oclock-swan-dive
      ;;zone-pgm-martini-swan-dive
      zone-pgm-rat-race
      zone-pgm-paragraph-spaz
      ;;zone-pgm-stress
      ;;zone-pgm-stress-destress
      zone-pgm-random-life
      ])

  (run-with-idle-timer 300 t 'zone)
  (message "  11.4 Screensaver"))


(provide 'my-misc)

;;; my-misc.el ends here
