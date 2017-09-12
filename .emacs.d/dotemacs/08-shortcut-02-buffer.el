;;; 08-shortcut-02-buffer.el --- a config file for buffer shortcut

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
;; Version: 2.2
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage buffers]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-26 (2.2)
;;    update to new conf format
;; 2016-09-28 (2.1)
;;    modify shortcut for bookmarks
;; 2013-04-10 (2.0)
;;    add shortcut for helm mode + clean up
;; 2013-02-05 (1.9)
;;    add shortcut for auctex mode
;; 2012-12-27 (1.8)
;;    switch shortcut for special buffer + toggle compile window when bookmark
;;    jump
;; 2012-11-30 (1.7)
;;    add shortcut for special buffer like grep or vc-diff
;; 2012-08-01 (1.6)
;;    add hide ecb compile window with quiting diff and log
;; 2012-07-09 (1.5)
;;    add shortcut to quit diff mode
;; 2012-06-21 (1.4)
;;    remove old shortcut to switch between buffers
;; 2012-06-08 (1.3)
;;    add clearcase shortcuts
;; 2012-03-30 (1.2)
;;    translate comments in English + cleaning
;; 2012-03-23 (1.1)
;;    change kill this buffer shortcut to have logical ecb shortcut
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; close the current buffer
(global-set-key         (kbd "M-`")             'kill-this-buffer)
;;
;; show a list of buffers in a new window
(global-set-key         (kbd "C-x C-b")         'electric-buffer-list)

;; show the window of bookmark
(global-set-key         (kbd "C-c C-b")         'bookmark-bmenu-list)

;;;; go to the next buffer (like new editor which I never like it)
;;(global-set-key         [(control tab)]         'previous-user-buffer)
;;;;
;;;; go to the previous buffer (like new editor which I never like it)
;;(global-set-key         [(control backtab)]     'next-user-buffer)


(provide '08-shortcut-02-buffer)

;;; 08-shortcut-02-buffer.el ends here
