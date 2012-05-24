;;; display-speedbar.el --- a config file for speedbar setting

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

;; Keywords: config, display, speedbar
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.1
;; Created: October 2006
;; Last-Updated: March 2012

;;; Commentary:
;;
;; it's a pain because it open a new window but not integrated
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display-speedbar'

;;; Change Log:
;; 2012-03-28 (1.1)
;;    translate comments in English
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; new independent window from Emacs buffer/window and show current directory
;; ('f' key) (where buffer is) or buffer list ('b' key), 'space' key can show
;; function in C buffers and go with 'enter'
(set-variable speedbar-frame-plist
  (quote
    (
      width 50
      border-width 0
      internal-border-width 0
      unsplittable t
      default-toolbar-visible-p nil
      has-modeline-p nil
      menubar-visible-p nil)
    )
  )
(set-variable speedbar-track-mouse-flag nil)
(set-variable speedbar-use-images nil)

;;; display-speedbar.el ends here
