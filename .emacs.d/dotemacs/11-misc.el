;;; 11-misc.el --- a config file for misc settings

;; Copyright (c) 2006-2017 Claude Tete
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

;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 2.9
;; Created: October 2006
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [[HEADER.remove whitespace at end of line + define name, browser, shell, new
;; line at end of file, compression, column 78 alignment
;; ]]
;; [DEFAULT.t]

;;; Change Log:
;; 2017-09-11 (2.9)
;;    remove dictionary settings to move in flyspell mode settings
;; 2017-07-26 (2.8)
;;    update to new conf format
;; 2017-05-26 (2.7)
;;    remove diary option about org mode compatibility
;; 2013-05-07 (2.6)
;;    add condition for some minor options
;; 2013-04-12 (2.5)
;;    no require, use autoload for ispell (it takes 7s to search aspell.exe ...)
;; 2012-12-27 (2.4)
;;    update dot emacs path + do not sort bookmark
;; 2012-12-06 (2.3)
;;    remove cygwin shell variable (only EmacsW32)
;; 2012-07-09 (2.2)
;;    clean up + tabs stop = width of tab + do not kill scratch buffer + more
;;    setting for ispell
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
;; [[VARCOMMENT.!!!PRIVATE!!! all theses variable should be put in private file of profile
;; USERNAME: define user name
;; ]]
;; [VARIABLE.tqnr-profile-username "your beautiful name"]
(setq user-full-name tqnr-profile-username)
;; [COMMENT.!!!PRIVATE!!! End]

;; use compression
(auto-compression-mode t)

;; kexep history between session of emacs (even after close it) for Minibuffer
(savehist-mode 1)

;; check all variables and non-interactive functions with apropos
(setq apropos-do-all t)

;; [COMMENT.]
;; [VARCOMMENT.SPACE: remove useless space at the end of line]
;; [VARIABLE.tqnr-profile-remove-useless-ending-space t]
(when tqnr-profile-remove-useless-ending-space
  (add-hook 'write-file-hooks 'delete-trailing-whitespace))
;; [VARCOMMENT.END OF FILE: be sure that a new line is at the end of a file when it's saved]
;; [VARIABLE.tqnr-profile-always-new-line-at-end t]
(when tqnr-profile-always-new-line-at-end
  (setq require-final-newline t))
;; tab = x spaces
(setq-default default-tab-width 8)
(setq-default tab-width 8)
;; set tab stops based on default-tab-width
;(setq-default tab-stop-list (loop for i
;                                  from default-tab-width to 120
;                                  by default-tab-width
;                                  collect i))
;; backspace on whitespace turns to spaces and removes one
(setq backward-delete-char-untabify-method 'untabify)
;; [VARCOMMENT.COLUMN: fill-xxx is set with a width]
;; [VARIABLE.tqnr-profile-fill-column 80]
(custom-set-variables '(fill-column tqnr-profile-fill-column))

;;
;;; COMPLETION
;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)
;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)

;;
;;; SCRATCH BUFFER
;; don't insert instructions into the *scratch* buffer
(setq initial-scratch-message nil)
;;; Unkillable scratch buffer, thanks to TN
(add-hook 'kill-buffer-query-functions
  '(lambda ()
     (if (equal (buffer-name (current-buffer)) "*scratch*")
       (progn
         (delete-region (point-min) (point-max))
         nil)
       t)))

;; FIX BUG
;; remove warning about undo too long
(custom-set-variables
  '(warning-suppress-types (quote ((undo discard-info))))
)
;; to do not have bug `backup-directory-alist' for TRAMP files
(add-to-list 'backup-directory-alist
  (cons tramp-file-name-regexp nil))

;; [COMMENT.]
;; [VARCOMMENT.WEB: set browser to open url]
;; [VARIABLE.tqnr-profile-browser "firefox"]
(setq browse-url-generic-program tqnr-profile-browser)
(setq browse-url-browser-function 'browse-url-generic)

;;;; Annoying arrows mode: ring bell when toooo much same arrow move and suggest
;;;; new command to move
;;(require 'annoying-arrows-mode)
;;(global-annoying-arrows-mode)

;; [COMMENT.]
;; [[SUBCOMMENT.CALENDAR: set latitude/longitude + location + holidays + custom date in
;; Modeline lunar phase, sunrise/sunset, time, etc
;; ]]
;; [SUBSECTION.tqnr-section-misc-calendar nil]
(when tqnr-section-misc-calendar (message "    Calendar...")
  ;; start week with Monday in 'calendar'
  (defvar calendar-week-start-day 1)
  ;;
  ;; [[VARCOMMENT.!!!PRIVATE!!! all theses variable should be put in private file of profile
  ;; LOCALIZATION: set latitude/longitude + location + holidays + custom date in
  ;; Modeline lunar phase, sunrise/sunset, time etc
  ;; ]]
  ;; [VARIABLE.tqnr-profile-latitude 0.00]
  (setq calendar-latitude tqnr-profile-latitude)
  ;; [VARIABLE.tqnr-profile-longitude 0.00]
  (setq calendar-longitude tqnr-profile-longitude)
  ;; [VARIABLE.tqnr-profile-location-name "Neverland"]
  (setq calendar-location-name tqnr-profile-location-name)
  ;; [COMMENT.!!!PRIVATE!!! End]

  (custom-set-variables
    ;; do not show holidays by default
    '(calendar-view-holidays-initially-flag nil)
    ;; show diary mark in calendar
    ;'(calendar-mark-diary-entries-flag t)
    )

  ;; [VARCOMMENT.FRENCH CALENDAR: set French holidays and day/month/moon phase name]
  ;; [VARIABLE.tqnr-section-misc-calendar-french t]
  (when tqnr-section-misc-calendar-french (message "      French calendar...")
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
    (message "      French calendar...done")
    ) ; (when section-misc-calendar-french

  ;; TRY
  ;; Diary (by Marc Tommasi)
;;  (setq view-diary-entries-initially t
;;    diary-file (concat (file-name-as-directory dotemacs-path) "diary")
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

  (message "    Calendar... Done"))


;; [SUBCOMMENT.BOOKMARK: default file, each command to add/modify bookmark save bookmark file]
;; [SUBSECTION.tqnr-section-misc-bookmark t]
(when tqnr-section-misc-bookmark (message "    Bookmarks...")
  ;; where to save the bookmarks
  (setq bookmark-default-file (concat (file-name-as-directory dotemacs-path) "bookmarks"))
  ;;
  ;; each command that sets a bookmark will also save your bookmarks
  (setq bookmark-save-flag 1)
  ;; [VARCOMMENT.BOOKMARK SORT: sort or do not sort bookmark when saving bookmark file]
  ;; [VARIABLE.tqnr-profile-bookmark-sort nil]
  (setq bookmark-sort-flag tqnr-profile-bookmark-sort)
  (message "    Bookmarks... Done"))

;; [SUBCOMMENT.SCREENSAVER: when idle for 5min some animations on buffer text]
;; [SUBSECTION.tqnr-section-misc-screensaver t]
(when tqnr-section-misc-screensaver (message "    Screensaver...")
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
  (message "    Screensaver... Done"))


(provide '11-misc)

;;; 11-misc.el ends here
