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
;; Version: 1.2
;; Created: October 2006
;; Last-Updated: July 2011

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-misc'
;;              var     `section-environment-os-recognition'

;;; Change Log:
;; 2011-07-25 (1.2)
;;    add test about ms windows for cygwin shell
;; 2011-04-21 (1.1)
;;    add user name + longitude latitude + calendar
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; supprime les espaces inutile en fin de ligne
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; defini le nom de l'utilisateur
(setq user-full-name "Claude TETE")

;; use compression
(auto-compression-mode t)

;; s'assure que une nouvelle ligne est presente a la fin du fichier lors de la
;; sauvegarde
(setq require-final-newline t)
;;
;; ne pas avoir l'incidence de `backup-directory-alist' pour TRAMP files
(add-to-list 'backup-directory-alist
  (cons tramp-file-name-regexp nil))

;; permet de justifier un paragraphe a la colonne 78
(custom-set-variables
  '(fill-column 78))

;; Set web browser to use
(setq browse-url-generic-program "opera")
(setq browse-url-browser-function 'browse-url-w3)


;;; CALENDAR
;; REQUIREMENT: var     `clt-working-environment'
(when section-misc-calendar (message "  11.1 Calendar...")
  ;; la semaine commence le lundi dans 'calendar'
  (defvar calendar-week-start-day 1)
  ;;
  ;; Lever/Coucher du soleil
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

  ;;(setq calendar-date-display-form ((if dayname (concat dayname ", ")) day " " monthname " " year))
  ;;(setq calendar-time-display-form (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

  (setq calendar-christian-all-holidays-flag t)
  (setq calendar-bahai-all-holidays-flag nil)
  (setq calendar-hebrew-all-holidays-flag nil)
  (setq calendar-islamic-all-holidays-flag nil)

  (custom-set-variables
    '(calendar-date-display-form
       (quote ((if dayname
                 (concat dayname " ")) day " " monthname " " year)))
    '(calendar-day-name-array
       ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])
    '(calendar-mark-holidays-flag t)
    '(calendar-month-name-array
       ["Janvier" "Fevrier" "Mars"
         "Avril" "Mai" "Juin"
         "Juillet" "Aout" "Septembre"
         "Octobre" "Novembre" "Decembre"])
    '(calendar-time-display-form
             (quote (24-hours ":" minutes
                      (if time-zone " (") time-zone (if time-zone ")"))))
    '(calendar-view-holidays-initially-flag t)
    '(holiday-general-holidays
       (quote (
                (holiday-fixed 1 1 "Nouvel An")
                (holiday-fixed 4 1 "1er Avril")
                (holiday-fixed 5 1 "Jour du Travail")
                (holiday-fixed 5 8 "Armistice WW1")
                (holiday-float 6 0 3 "Fete des Peres")
                (holiday-float 5 0 2 "Fete des Meres")
                (holiday-fixed 7 14 "Prise de la Bastille")
                (holiday-fixed 8 15 "??")
                (holiday-fixed 11 1 "Toussaint")
                (holiday-fixed 11 11 "Armistice WW2")
                (holiday-fixed 12 25 "Noel"))))
    '(lunar-phase-names
       (quote ("Nouvelle Lune" "Premier Quartile" "Plein Lune" "Dernier Quartile")))

    )
  (message "  11.1 Calendar... Done"))

;;
;; configure le shell de cygwin
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
;; la langue du dictionnaire
;;(setq ispell-dictionary "english")

;;; misc.el ends here
