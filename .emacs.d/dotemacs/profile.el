;;; profile.el --- a config file for profile

;; Copyright (c) 2012 Claude Tete
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

;; Keywords: config, profile, environment, working
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.1
;; Created: June 2012
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment-profile'

;;; Change Log:
;; 2012-06-01 (0.1)
;;    creation from scratch


;;; Code:
;; add to load path the profile directory
(add-to-list 'load-path (concat dotemacs-path "/profile"))
(setq load-path (cons (expand-file-name (concat dotemacs-path "/profile")) load-path))

;;
;;; LOAD
;; the load of profile can be depended of name and system

;; ALSTOM TRANSPORT
(if (and (string= system-name "CWVBN16EWJ") (string= system-configuration "i386-mingw-nt5.1.2600"))
  (progn
    (setq profile "Alstom Transport")
    (try-require 'profile-alstom-transport "    ")
    ))

;;
;;; PERSONAL
;; see .emacs.d/profile/profile-default.el like an example to create one
;; add your profile "profile-my-profile.el" in the ".emacs.d/profile" folder and
;; load like this (put it just under this comment):
;; (try-require 'profile-my-profile "    ")
;;;;;;;;;;;;;;;;;;;
;; Between here ...

;(try-require 'profile-default "    ")

;; ... and here
;;;;;;;;;;;;;;;;;;;

;; show name of profile
(message (concat "* Profile: " profile))


(provide 'profile)

;;; profile.el ends here
