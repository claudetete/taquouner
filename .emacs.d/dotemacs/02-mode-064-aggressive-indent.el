;;; 02-mode-064-aggressive-indent.el --- configuration of aggressive indent mode

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
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.indent all line in function/condition in C or lisp mode when edit it]
;; [SUBDEFAULT.nil]


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/names-master"))
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/aggressive-indent-mode-master"))
(try-require 'aggressive-indent "    ")


(provide '02-mode-064-aggressive-indent)

;;; 02-mode-064-aggressive-indent.el ends here
