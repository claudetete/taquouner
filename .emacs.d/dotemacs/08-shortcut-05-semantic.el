;;; 08-shortcut-05-semantic.el --- a config file for semantic mode shortcut

;; Copyright (c) 2010-2019 Claude Tete
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
;; Version: 2.0
;; Created: October 2010
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to move in source code with semantic]
;; [SUBDEFAULT.t]


;;; Code:

;; move to the matched parenthesis
(global-set-key         (kbd "<M-right>")       'forward-sexp)
(global-set-key         (kbd "<M-left>")        'backward-sexp)


(provide '08-shortcut-05-semantic)

;;; 08-shortcut-05-semantic.el ends here
