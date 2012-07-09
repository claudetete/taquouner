;;; display-buffer.el --- a config file for window/buffer display setting

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, display, buffer, window
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.6
;; Created: October 2006
;; Last-Updated: July 2012

;;; Commentary:
;;
;; load by `dotemacs/display.el'
;; REQUIREMENT: var     `section-display-windows-buffers'

;;; Change Log:
;; 2012-07-09 (1.6)
;;    try new handler for completion buffer
;; 2012-06-14 (1.5)
;;    remove misunderstanding for same-window-buffer-names option
;; 2012-06-05 (1.4)
;;    move transparency to interface.el
;; 2012-05-04 (1.3)
;;    add three possibility of transparency
;; 2012-03-20 (1.2)
;;    translate comments in English
;; 2012-03-20 (1.1)
;;    add buffer name to list of compilation window
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; Define buffers that should appear in the same window than the window caller.
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "*Colors*")

;; display an arrows or a corner at left to show beginning and ending of a
;; file
(setq-default indicate-buffer-boundaries (quote left))

;; after a PageUp or Down, it will display 5 shared lines
(custom-set-variables
  '(next-screen-context-lines 5))

;;; do not work as expected
;;; (custom-set-faces
;;;   '(default ((t (:stipple "d:/wallpaper.png")))))

;;(add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))


(provide 'display-buffer)

;;; display-buffer.el ends here
