;;; externfiles.el --- a config file for load file (not mode)

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

;; Keywords: config, file, function
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.3
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-external'

;;; Change Log:
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
(when section-external-directory (message "  2.1 Directory .emacs.d...")
  ;; path to load mode
  (add-to-list 'load-path dotemacs-path)
  (setq load-path (cons (expand-file-name dotemacs-path) load-path))
  (add-to-list 'load-path  (concat dotemacs-path "/plugins"))
  (setq load-path (cons (expand-file-name (concat dotemacs-path "/plugins")) load-path))
  (message "  2.1 Directory .emacs.d ... Done"))

;;
;;; FONCTIONS
;; REQUIREMENT: var     `section-external-functions'
(when section-external-functions (message "  2.2 Functions perso...")
  (load-file (concat dotemacs-path "/dotemacs/functions.el"))
  (message "  2.2 Functions perso... Done"))

;;
;;; VECTRA
;; REQUIREMENT: var     `section-external-vectra'
(when section-external-vectra (message "  2.3 Vectra...")
;; Vectra man et doc (reste pas tres utile)
  (load-file (concat dotemacs-path "/plugins/vectra.el"))
  (message "  2.3 Vectra... Done"))

;;
;;; SETNU
;; REQUIREMENT: var     `section-external-setnu'
(when section-external-setnu (message "  2.4 Setnu...")
  ;; affichage des numeros de lignes (deprecated exist in emacs)
  (load-file (concat dotemacs-path "/plugins/setnu.el"))
  (message "  2.4 Setnu... Done"))

;;
;;; HOME/END
;; REQUIREMENT: var     `section-external-home-end'
(when section-external-home-end (message "  2.5 Home/End...")
  ;; pour avoir plus de fonctionnalites avec les touches home et end
  (load-file (concat dotemacs-path "/plugins/pc-keys.elc"))
  (message "  2.5 Home/End... Done"))

;;; externfiles.el ends here
