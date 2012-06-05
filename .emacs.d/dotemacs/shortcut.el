;;; shortcut.el --- a config file for shortcut

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

;; Keywords: config, shortcut, emacs
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.0
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-global'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-06-05 (1.0)
;;    split from emacs.el
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; GLOBAL
(when section-shortcut-global (message "  8.1 Global Shortcuts...")
  (try-require 'shortcut-global "    ")
  (message "  8.1 Global Shortcuts... Done"))
;;
;; WINDOWS
(when section-shortcut-windows (message "  8.2 Windows Shortcuts...")
  (try-require 'shortcut-windows "    ")
  (message "  8.2 Windows Shortcuts... Done"))
;;
;; BUFFERS
(when section-shortcut-buffers (message "  8.3 Buffers Shortcuts...")
  (try-require 'shortcut-buffers "    ")
  (message "  8.3 Buffers Shortcuts... Done"))
;;
;; ECB
(when section-shortcut-ecb (message "  8.4 Ecb Shortcuts...")
  (try-require 'shortcut-ecb "    ")
  (message "  8.4 Ecb Shortcuts... Done"))
;;
;; GREP
(when section-shortcut-grep (message "  8.5 Grep Shortcuts...")
  (try-require 'shortcut-grep "    ")
  (message "  8.5 Grep Shortcuts... Done"))
;;
;; FUNCTION
(when section-shortcut-function (message "  8.6 Functions Shortcuts...")
  (try-require 'shortcut-function "    ")
  (message "  8.6 Functions Shortcuts... Done"))
;;
;; TAGS
(when section-shortcut-tags (message "  8.7 Tags Shortcuts...")
  (try-require 'shortcut-tags "    ")
  (message "  8.7 Tags Shortcuts... Done"))
;;
;; SEMANTIC
(when section-shortcut-semantic (message "  8.8 Semantic Shortcuts...")
  (try-require 'shortcut-semantic "    ")
  (message "  8.8 Semantic Shortcuts... Done"))


(provide 'shortcut)

;;; shortcut.el ends here
