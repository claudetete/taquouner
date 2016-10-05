;;; project.el --- a config file for ede project settings

;; Copyright (c) 2011-2016 Claude Tete
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

;; Keywords: config, ede, cedet, project, ide
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.7
;; Created: July 2011
;; Last-Updated: September 2016

;;; Commentary:
;;
;; load by `mode.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-mode-cedet'
;;              and cedet has been successfully loaded
;;
;; to had new project you can see `.emacs.d/myproject.ede.el' as example
;; and feed each new ede file project into the variable `profile-ede-project'
;; from `.emacs.d/profile/profile-yourprofile.el'
;; (see `.emacs.d/profile/profile-default.el' as example)

;;; Change Log:
;; 2016-09-28 (1.7)
;;    EDE can be disabled
;; 2012-12-06 (1.6)
;;    error not in message but in warning buffer
;; 2012-07-09 (1.5)
;     add new comments
;; 2012-06-05 (1.4)
;;    remove project and use a list of it in profile file
;; 2012-05-07 (1.3)
;;    add project PUMA for AT
;; 2012-03-02 (1.2)
;;    add project for AT
;; 2011-09-02 (1.1)
;;    add project for MM
;; 2011-08-13 (1.0)
;;    use external file for define project
;; 2011-07-10 (0.1)
;;    creation from scratch (no history since)


;;; Code:

;;
;;; EDE PROJECT
(when section-mode-cedet-ecb
  (while profile-ede-project
    (let (file)
      (setq file (pop profile-ede-project))
      (if (file-exists-p file)
        (load-file file)
        (display-warning 'profile-ede-project (format "file not found: %s" file) :warning)
        )
      )
    )
  )

(provide 'project)

;;; project.el ends here
