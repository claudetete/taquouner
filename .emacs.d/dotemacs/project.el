;;; project.el --- a config file for ede project settings

;; Copyright (c) 2011, 2012 Claude Tete
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
;; Version: 1.4
;; Created: July 2011
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `mode-semantic.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-mode-cedet-semantic'
;;              var     `section-mode-cedet'

;;; Change Log:
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
(while profile-ede-project
  (let (file)
    (setq file (pop profile-ede-project))
    (if (file-exists-p file)
      (load-file file)
      (message "** file not found: %s" file)
      )
    )
  )


(provide 'project)

;;; project.el ends here
