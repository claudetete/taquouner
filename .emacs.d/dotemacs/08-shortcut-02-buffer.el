;;; 08-shortcut-02-buffer.el --- a config file for buffer shortcut

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
;; Version: 2.2
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage buffers]
;; [SUBDEFAULT.t]


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
