;;; 01-function-15-rtrt.el --- a config file to add some function for rtrt script

;; Copyright (c) 2012-2019 Claude Tete
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
;; Version: 1.6
;; Created: March 2012
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about RTRT script .ptu file]
;; [SUBDEFAULT.nil]


;;; Code:
(when tqnr-section-mode-rtrt-script

  ;;
  ;; ALIGN RTRT MODE
  ;; align "init" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-init (start end)
    "Align init variable test case (between START and END)."
    (interactive (tqnr-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\binit\\(\\s-*[=(]\\|\\s-+in\\b\\|\\s-+from\\b\\)") 1 1)
    )

  ;; align "expected value" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-ev (start end)
    "Align expected value variable (between START and END)."
    (interactive (tqnr-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "\\bev\\(\\s-*[=(]\\|\\s-+in\\b\\)") 1 1)
    )

  ;; align "expected value" and "init" in ptu script for rtrt (by Claude TETE)
  (defun rtrt-align-declaration (start end)
    "Align variable (between START and END)."
    (interactive (tqnr-get-paragraph-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (rtrt-align-init (point-min) (point-max))
        (rtrt-align-ev (point-min) (point-max))
        )
      )
    )

  ;; align "=" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-set (start end)
    "Align = (between START and END)."
    (interactive (tqnr-get-paragraph-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "[-\\*/&|+=]?=") 1 1)
    )

  ;; align stub call "digit =>" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-align-stub (start end)
    "Align => (between START and END)."
    (interactive (tqnr-get-paragraph-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (align-regexp start end (concat "\\(\\s-*\\)" "[0-9]+\\(\\.\\.[0-9]\\)?\\s-*=>") 1 1)
    )

  ;;
  ;; FORMAT RTRT MODE
  ;; remove whitespace before a colon in ptu script for RTRT (by Claude TETE)
  (defun rtrt-remove-whitespace-before-colon (start end)
    "Remove all space before a colon (between START and END)."
    (interactive (tqnr-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (replace-regexp "\\s-+," "," nil (point-min) (point-max))
        (replace-regexp ",\\(\\sw\\)" ", \\1" nil (point-min) (point-max))
        (replace-regexp "\\([a-zA-Z0-9]\\)=" "\\1 =" nil (point-min) (point-max))
        (replace-regexp "=\\([-a-zA-Z0-9&({]\\)" "= \\1" nil (point-min) (point-max))
        )
      )
    )

  ;; upcase "var" in ptu script for RTRT (by Claude TETE)
  (defun rtrt-upcase-var-string (start end)
    "Upcase the var string (between START and END)."
    (interactive (tqnr-get-line-position))
    (unless (and start end)
      (error "The mark is not set now, so there is no region"))
    (replace-regexp " [vV]ar " " VAR " nil start end)
    )

  ;;
  ;; MOVE
  ;; move to the previous up heading (by Claude TETE)
  (defun rtrt-up-heading ()
    "Go up in script SERVICE"
    (interactive)
    (re-search-backward "^\\s-*SERVICE\\b")
    (when (looking-at "[ ]") (forward-char 2)))

  ;; move to the previous up heading (by Claude TETE)
  (defun rtrt-down-heading ()
    "Go down in script SERVICE"
    (interactive)
    (re-search-forward "^\\s-*END SERVICE\\b")
    (next-line))

  ;; move to the previous TEST header (by Claude TETE)
  (defun rtrt-up-test-header ()
    "Go up in script from TEST to TEST"
    (interactive)
    (re-search-backward "^\\s-*TEST\\b")
    (forward-char 2))

  ;; move to the next TEST footer (by Claude TETE)
  (defun rtrt-down-test-header ()
    "Go down in script from END TEST to END TEST"
    (interactive)
    (re-search-forward "^\\s-*END TEST\\b")
    (move-beginning-of-line nil)
    (next-line))

  ;;
  ;; RTP
  ;; move to previous unit testing (by Claude TETE)
  (defun rtrt-rtp-up-heading ()
    "Go up to unit testing"
    (interactive)
    (re-search-backward "<unit_testing>")
    (move-beginning-of-line nil))

  ;; move to next unit testing (by Claude TETE)
  (defun rtrt-rtp-down-heading ()
    "Go down to unit testing"
    (interactive)
    (re-search-forward "</unit_testing>")
    (move-end-of-line nil)
    (forward-char 1))

  ) ; (when tqnr-section-mode-rtrt-script


(add-hook 'rtrt-script-mode-hook
  '(lambda ()
     ;; move
     (local-set-key     (kbd "<M-left>")        'rtrt-up-test-header)
     (local-set-key     (kbd "<M-right>")       'rtrt-down-test-header)
     (local-set-key     (kbd "<M-up>")          'rtrt-up-heading)
     (local-set-key     (kbd "<M-down>")        'rtrt-down-heading)
     ;; use align regexp for .ptu file (rtrt script)
     (local-set-key     (kbd "C-c p o")         'rtrt-align-init)
     (local-set-key     (kbd "C-c p ;")         'rtrt-align-ev)
     (local-set-key     (kbd "C-c p [")         'rtrt-align-declaration)
     (local-set-key     (kbd "C-c p =")         'rtrt-align-set)
     (local-set-key     (kbd "C-c p s")         'rtrt-align-stub)
     ;; format the .ptu file (rtrt script)
     (local-set-key     (kbd "C-c r v")         'rtrt-upcase-var-string)
     (local-set-key     (kbd "C-c r s")         'rtrt-remove-whitespace-before-colon)
     ))

(add-hook 'nxml-mode-hook
  (lambda ()
    (when (string-match "\\.rtp$" (buffer-name))
      (local-set-key   (kbd "<M-left>")        'rtrt-rtp-up-heading)
      (local-set-key   (kbd "<M-right>")       'rtrt-rtp-down-heading))))

(provide '01-function-15-rtrt)

;;; 01-function-15-rtrt.el ends here
