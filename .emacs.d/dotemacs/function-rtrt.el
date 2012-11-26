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
;; Version: 1.2
;; Created: March 2012
;; Last-Updated: November 2012

;;; Commentary:
;;
;; load by `functions.el'
;; REQUIREMENT: var     `section-mode-rtrt-script'

;;; Change Log:
;; 2012-11-26 (1.2)
;;    fix behaviour with align functions
;; 2012-10-31 (1.1)
;;    add new function to move in rtrt files
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
    (align-regexp start end (concat "\\(\\s-*\\)" "\\binit[ ]*[=if]") 1 1)
    )
  ;;; align "expected value" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-ev (start end)
    "Align expected value variable (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\bev[ ]*[=i(]") 1 1)
    )
  ;;; align "expected value" and "init" in ptu script for rtrt (by Claude TETE)
  (defun rtrt-align-declaration (start end)
    "Align variable (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (rtrt-align-init start end)
    (rtrt-align-ev start end) ; sometimes it bugs the last ev is not align ??
    )
  ;;; align "=" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-set (start end)
    "Align = (between START and END)."
    (interactive (clt-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "[-+=]?=") 1 1)
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

;;
;;;
;;;; MOVE
  ;;; move to the previous up heading (by Claude TETE)
  (defun rtrt-up-heading ()
    "Go up in script SERVICE->TEST->ELEMENT"
    (interactive)
    (find-matching-element 're-search-backward 0 "  END \\(\\b\\(ELEMENT\\|TEST\\|SERVICE\\)\\b\\)" "\\(  ELEMENT\\|  TEST\\|^SERVICE\\)\\b")
    (when (looking-at "[ ]") (forward-char 2)))
  ;;; move to the previous up heading (by Claude TETE)
  (defun rtrt-down-heading ()
    "Go down in script SERVICE"
    (interactive)
    (re-search-forward "^END SERVICE\\b")
    (next-line))
  ;;; move to the previous TEST header (by Claude TETE)
  (defun rtrt-up-test-header ()
    "Go up in script from TEST to TEST"
    (interactive)
    (re-search-backward "  TEST\\b")
    (forward-char 2))
  ;;; move to the next TEST footer (by Claude TETE)
  (defun rtrt-down-test-header ()
    "Go down in script from END TEST to END TEST"
    (interactive)
    (re-search-forward "  END TEST\\b")
    (move-beginning-of-line nil)
    (next-line))

;;
;;;
;;;; RTP
  ;;; move to previous unit testing (by Claude TETE)
  (defun rtrt-rtp-up-heading ()
    "Go up to unit testing"
    (interactive)
    (re-search-backward "<unit_testing>")
    (move-beginning-of-line nil))
  ;;; move to next unit testing (by Claude TETE)
  (defun rtrt-rtp-down-heading ()
    "Go down to unit testing"
    (interactive)
    (re-search-forward "</unit_testing>")
    (move-end-of-line nil)
    (forward-char 1))

  ) ; (when section-mode-rtrt-script


(provide 'function-rtrt)

;;; function-rtrt.el ends here
