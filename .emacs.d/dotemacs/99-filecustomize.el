;;; 99-filecustomize.el --- a config file for misc settings

;; Copyright (c) 2006-2019 Claude Tete
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
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [[HEADER.all customize settings are put in here when you use interface
;; (customize) to change settings
;; ]]
;; [DEFAULT.t]


;;; Code:
;; customize modification (made by Emacs interface) are put in tqrn-custom.el
(setq custom-file (concat (file-name-as-directory dotemacs-path) "dotemacs/tqnr-custom.el"))
;; load it to apply modification in it
(try-require 'tqnr-custom "    ")


(provide '99-filecustomize)

;;; 99-filecustomize.el ends here
