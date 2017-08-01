;;; 02-mode.el --- configuration of mode

;; Copyright (c) 2017 Claude Tete
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
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [HEADER.load extern files which are modes in plugins/]

;;; Change Log:
;; 2017-07-25 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:

;; path to load mode
(add-to-list 'load-path  (concat (file-name-as-directory tqnr-dotemacs-path) "plugins"))


(provide '02-mode)

;;; 02-mode.el ends here
