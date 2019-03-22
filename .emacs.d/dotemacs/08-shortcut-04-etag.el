;;; 08-shortcut-04-etag.el --- a config file for tags shortcut

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
;; Version: 2.4
;; Created: September 2010
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage gtags or etags]
;; [SUBDEFAULT.t]


;;; Code:

;; completion with tag file (show a list)
(global-set-key         (kbd "C-/")             'complete-tag)
;; search in tag file
(global-set-key         (kbd "C-c ,")           'tags-search)
;; next result for tag search
(global-set-key         (kbd "C-,")             'tags-loop-continue)

  ;; previous result for tag search
(global-set-key         (kbd "C->")             'pop-tag-mark)


(provide '08-shortcut-04-etag)

;;; 08-shortcut-04-etag.el ends here
