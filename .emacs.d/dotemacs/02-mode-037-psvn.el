;;; 02-mode-037-psvn.el --- configuration of psvn mode

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.add an icon in modeline where color give status of SVN]
;; [SUBDEFAULT.nil]


;;; Code:
;; it add a small icon in modeline where color give status of SVN
;; + show all file status of a directory
(use-package psvn
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/psvn.el")))


(provide '02-mode-037-psvn)

;;; 02-mode-037-psvn.el ends here
