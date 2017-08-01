;;; 02-mode-027-google-calendar.el --- configuration of google calendar mode

;; Copyright (c) 2017 Claude Tete
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
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.to import Google calendar]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; can import google calendar in Emacs calendar
(when (try-require 'icalendar "    ")
  (when (try-require 'google-calendar "    ")
    ;; [VARCOMMENT.!!!PRIVATE!!! all theses variable should be put in private file of profile]
    ;; [VARIABLE.tqnr-profile-google-calendar-user "name@email.com"]
    (setq google-calendar-user           tqnr-profile-google-calendar-user)
    ;; [VARIABLE.tqnr-profile-google-calendar-src (concat tqnr-dotemacs-path "/plugins/google")]
    (setq google-calendar-code-directory tqnr-profile-google-calendar-src)
    ;; [VARIABLE.tqnr-profile-google-calendar-directory "~/tmp"]
    (setq google-calendar-directory      tqnr-profile-google-calendar-directory)
    ;; [VARIABLE.tqnr-profile-google-calendar-url "https://www.google.com/calendar/ical..."]
    (setq google-calendar-url            tqnr-profile-google-calendar-url)

    (setq google-calendar-auto-update    t)
    (google-calendar-download)
    ))


(provide '02-mode-027-google-calendar)

;;; 02-mode-027-google-calendar.el ends here
