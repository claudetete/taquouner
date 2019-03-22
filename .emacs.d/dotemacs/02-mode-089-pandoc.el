;;; 02-mode-089-pandoc.el --- configuration of pandoc-mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Created: December 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.PanDoc tools mode to translate between markup syntax]
;; [SUBDEFAULT.nil]
;;
;; TODO: try using it in MS Windows


;;; Code:

(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/pandoc-mode/"))
(when (try-require 'pandoc-mode "    ")
  (add-hook 'fitnesse-mode-hook 'pandoc-mode)
  )


(provide '02-mode-089-pandoc)

;;; 02-mode-089-pandoc.el ends here
