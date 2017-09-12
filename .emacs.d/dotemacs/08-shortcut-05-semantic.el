;;; 08-shortcut-05-semantic.el --- a config file for semantic mode shortcut

;; Copyright (c) 2010-2017 Claude Tete
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
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to move in source code with semantic]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-26 (2.0)
;;    update to new conf format
;; 2017-05-26 (1.9)
;;    try logical shortcut about smartparens
;; 2016-09-28 (1.8)
;;    change condition about cedet and helm
;; 2014-03-26 (1.7)
;;    modify helm-imenu shortcut
;; 2013-05-07 (1.6)
;;    add shortcut for smart-forward mode
;; 2013-04-10 (1.5)
;;    add helm imenu shortcut
;; 2012-08-01 (1.4)
;;    fix bug with Shift+Click, Back on mouse work with semantic, right click
;;    display a menu like ecb method
;; 2012-05-28 (1.3)
;;    add new  shortcut to 'grep' in project all occurences
;; 2012-03-28 (1.2)
;;    change shortcut and add go/back to the tag
;; 2012-03-20 (1.1)
;;    add shortcut for tags
;; 2011-08-10 (1.0)
;;    update shortcut to use the colon
;; 2011-07-25 (0.1)
;;    creation from scratch (no history since)


;;; Code:

;; move to the matched parenthesis
(global-set-key         (kbd "<M-right>")       'forward-sexp)
(global-set-key         (kbd "<M-left>")        'backward-sexp)


(provide '08-shortcut-05-semantic)

;;; 08-shortcut-05-semantic.el ends here
