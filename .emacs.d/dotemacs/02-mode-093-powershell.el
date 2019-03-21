;;; 02-mode-093-powershell.el --- configuration of powershell mode

;; Copyright (c) 2018 Claude Tete
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
;; Created: May 2018
;; Last-Updated: May 2018

;;; Commentary:
;;
;; [SUBHEADER.Powershell mode]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2018-05-09 (0.1)
;;    creation from scratch


;;; Code:

(when (try-require 'powershell "    ")
  (setq powershell-indent 2)
  )

(provide '02-mode-093-powershell)

;;; 02-mode-093-powershell.el ends here
