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
;; Version: 0.5
;; Created: June 2012
;; Last-Updated: December 2012

;;; Commentary:
;;
;; load by `dotemacs/environment.el'
;; REQUIREMENT: var     `section-environment-profile'

;;; Change Log:
;; 2012-12-05 (0.5)
;;    change for the simplest way to do
;; 2012-10-24 (0.4)
;;    add portable profile
;; 2012-07-09 (0.3)
;;    add new condition for AT + comments
;; 2012-06-05 (0.2)
;;    put profile name in profile file
;; 2012-06-01 (0.1)
;;    creation from scratch


;;; Code:
;; add to load path the profile directory
(add-to-list 'load-path (concat dotemacs-path "/profile"))

;;
;;; LOAD
(let ((profile-file (concat dotemacs-path "/profile/profile-" profile-name ".el")))
  ;; load only if the file exists
  (if (file-exists-p profile-file)
    (load profile-file)
    (progn
      (display-warning 'profile-name (concat profile-name " profile do not exist") :warning)
      (display-warning 'profile-name (concat "file not found: " profile-file) :warning)
      ) ; (progn
    ) ; (if (file-exists-p profile-file)
  )

;; show name of profile
(message (concat "* Profile: " profile-name))


(provide 'profile)

;;; profile.el ends here
