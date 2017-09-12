;;; 08-shortcut-04-etag.el --- a config file for tags shortcut

;; Copyright (c) 2006-2017 Claude Tete
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
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage gtags or etags]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-26 (2.4)
;;    update to new conf format
;; 2017-05-26 (2.3)
;;    fix gtags and projectile with helm shortcuts
;; 2016-09-28 (2.2)
;;    add helm/gtags and helm/projectile shortcuts
;; 2014-03-26 (2.1)
;;    modify gtags find file function shortcut
;; 2013-04-10 (2.0)
;;    change find file shortcut + clean up
;; 2012-08-01 (1.9)
;;    move some shortcuts
;; 2012-07-19 (1.8)
;;    hide ecb compile window after select a tag + condition when ecb is enable
;; 2012-06-12 (1.7)
;;    remove old way to bind keys
;; 2012-05-28 (1.6)
;;    do not use gtags find with grep with cedet (replaced see
;;    shortcut-semantic.el)
;; 2012-05-04 (1.5)
;;    add shortcuts to search symbol assignation
;; 2012-03-30 (1.4)
;;    translate comments in English
;; 2012-03-02 (1.3)
;;    add condition about semantic
;; 2011-08-10 (1.2)
;;    update shortcut to use the colon
;; 2011-07-08 (1.1)
;;    add shortcuts for gtags
;; 2010-10-13 (1.0)
;;    split .emacs file
;; 2010-09-11 (0.1)
;;    creation from scratch (no history since)


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
