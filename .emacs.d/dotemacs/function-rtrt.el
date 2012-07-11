;;; function-rtrt.el --- a config file to add some function for rtrt script

;; Copyright (c) 2012 Claude Tete
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

;; Keywords: config, function, rtrt
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.0
;; Created: March 2012
;; Last-Updated: July 2012

;;; Commentary:
;;
;; load by `functions.el'
;; REQUIREMENT: var     `section-mode-rtrt-script'

;;; Change Log:
;; 2012-07-11 (1.0)
;;    split from functions.el


;;; Code:
(when section-mode-rtrt-script
;;
;;;
;;;; ALIGN RTRT MODE
  ;;; align "init" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-init (start end)
    "Align init variable test case (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\binit ") 1 1)
    )
  ;;; align "expected value" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-ev (start end)
    "Align expected value variable (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\bev ") 1 1)
    )
  ;;; align "expected value" and "init" in ptu script for rtrt (by Claude TETE)
  (defun rtrt-align-declaration (start end)
    "Align variable (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (rtrt-align-init start end)
    (rtrt-align-ev start end) ; sometimes it bugs the last ev is not align or two line after region is align
    )
  ;;; align "=" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-set (start end)
    "Align = (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "[-+=]\\{0,1\\}=") 1 1)
    )

;;
;;;
;;;; FORMAT RTRT MODE
  ;;; remove whitespace before a colon in ptu script for RTRT (by Claude TETE)
  (defun rtrt-remove-whitespace-before-colon (start end)
    "Remove all space before a colon (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (replace-regexp "\\s-+," "," nil start end)
    )
  ;;; upcase "var" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-upcase-var-string (start end)
    "Upcase the var string (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (replace-regexp " [vV]ar " " VAR " nil start end)
    )

  ) ; (when section-mode-rtrt-script



(provide 'function-rtrt)

;;; function-rtrt.el ends here
