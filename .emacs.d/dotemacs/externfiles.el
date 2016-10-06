;;; externfiles.el --- a config file for load file (not mode)

;; Copyright (c) 2006-2016 Claude Tete
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

;; Keywords: config, file, function
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.6
;; Created: October 2006
;; Last-Updated: October 2016

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-external'

;;; Change Log:
;; 2016-10-06 (1.6)
;;    remove .emacs.d from load-path
;; 2012-12-27 (1.5)
;;    update dot emacs path
;; 2012-06-05 (1.4)
;;    change load to require to have nicer *Messages* + remove
;;    deprecated code
;; 2012-03-28 (1.3)
;;    translate comments in English
;; 2012-03-02 (1.2)
;;    change hard path by a variable + remove ms windows
;; 2011-07-09 (1.1)
;;    remove loading of custom file
;; 2011-04-21 (1.1)
;;    add test about ms windows for load path
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch  + some other function files (no history since)


;;; Code:
;;; DIRECTORY
;; REQUIREMENT: var     `section-external-directory'
(when section-external-directory (message "  1.1 Directory .emacs.d...")
  ;; path to load mode
  (add-to-list 'load-path  (concat (file-name-as-directory dotemacs-path) "plugins"))
  (message "  1.1 Directory .emacs.d ... Done"))

;;
;;; FUNCTIONS
;; REQUIREMENT: var     `section-external-functions'
(when section-external-functions (message "  1.2 Functions custom...")
  (try-require 'functions "    ")
  (message "  1.2 Functions custom... Done"))

;;
;;; VECTRA
;; REQUIREMENT: var     `section-external-vectra'
(when section-external-vectra (message "  1.3 Vectra...")
;; Vectra man and doc (the rest is not very useful)
  (try-require 'vectra "    ")
  (message "  1.3 Vectra... Done"))

;;
;;; HOME/END
;; REQUIREMENT: var     `section-external-home-end'
(when section-external-home-end (message "  1.4 Home/End...")
  ;; to add features to home/end key (two push will get you at the end/start
  ;; of display) (three push will get you at the end/start of buffer)
  (try-require 'pc-keys "    ")
  (message "  1.4 Home/End... Done"))


(provide 'externfiles)

;;; externfiles.el ends here
