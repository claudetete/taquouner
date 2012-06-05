;;; display.el --- a config file for display setting

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
;; Version: 1.0
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-display'

;;; Change Log:
;; 2012-06-05 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;
;; WINDOWS/BUFFERS
(when section-display-windows-buffers (message "  5.1 Windows / Buffers...")
  (try-require 'display-buffer "    ")
  (message "  5.1 Windows / Buffers... Done"))
;;
;; SPEEDBAR
(when section-display-speedbar (message "  5.2 SpeedBar...")
  (try-require 'display-speedbar "    ")
  (message "  5.2 SpeedBar... Done"))
;;
;; FONT
(when section-display-font (message "  5.3 Font...")
  (try-require 'display-font "    ")
  (message "  5.3 Font... Done"))
;;
;; COLOR
(when section-display-color (message "  5.4 Color...")
  (try-require 'display-color "    ")
  (message "  5.4 Color... Done"))


(provide 'display)

;;; display-buffer.el ends here
