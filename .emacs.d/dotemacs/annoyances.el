;;; annoyances.el --- a config file for all annoying things

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

;; Keywords: config, annoying
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.7
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-annoyances'

;;; Change Log:
;; 2012-06-14 (1.8)
;;    clean up
;; 2012-06-12 (1.7)
;;    add section for scroll line by line
;; 2012-06-05 (1.6)
;;    autosave and backup directory are put in profile files when
;;    nothing is set it take the temporary system folder
;; 2012-04-16 (1.5)
;;    add working environment condition
;; 2012-03-30 (1.4)
;;    comments in English + preserve cursor
;; 2012-03-21 (1.3)
;;    add directory for backup file
;; 2012-03-02 (1.2)
;;    add confirm kill + scroll preserve + backup file
;; 2011-07-07 (1.1)
;;    remove shortcut Control+Middle mouse button
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; remove home page message
(setq inhibit-startup-message t)

;; all answer will be y or n
;; no more mix between y/yes and n/no
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; do not ask confirmation when I want refresh buffer
(setq revert-without-query '(".*"))

;;
;; remove insert shortcut, no more insertion mode
(global-set-key [insert] 'ignore)
(put 'overwrite-mode 'disabled t)
;;
;; disable shortcut C-PageUp & C-PageDown
(global-set-key (kbd "<C-next>") 'ignore)
(global-set-key (kbd "<C-prior>") 'ignore)

;;
;; no more dialog boxes
(setq use-dialog-box nil)
;;
;; no more dialog boxes for files
(setq use-file-dialog nil)

;; no more tooltips (delay of 9999 seconds before displayed)
(setq tooltip-delay 9999)

;;;; ask confirmation to quit Emacs
;;;; do not use it's a pain
;;(setq confirm-kill-emacs t)

;; enable up/down case region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable narrow to region (C-x n n) (C-x n w)
;; you can display only the region and do not mess up with rest
(put 'narrow-to-region 'disabled nil)

;; enable goal column (C-x C-n) (C-u C-x C-n)
;; the point will stick with the set column when you go up/down
(put 'set-goal-column 'disabled nil)

;; preserve cursor when scrolling
(setq scroll-preserve-screen-position t)

;;
;;;; TRUNCATE LINE
;; REQUIREMENT: var     `section-annoyances-truncate-line'
(when section-annoyances-truncate-line (message "  10.1 Truncate Line...")
  ;; do not truncate line
  ;; a displayed row is a line but can be hidden outside window (need to
  ;; scroll horizontally)
  (setq-default truncate-lines nil)
  (message "  10.1 Truncate Line... Done"))

;;
;;;; SCROLL PRESERVE CURSOR POSITION
;; REQUIREMENT: var     `section-annoyances-scroll-preserve-cursor-position'
(when section-annoyances-scroll-preserve-cursor-position (message "  10.2 Scroll preserve cursor position...")
  ;; return to same line on a scroll back
  (setq scroll-preserve-screen-position t)
  (message "  10.2 Scroll preserve cursor position... Done"))

;;
;;;; NO BACKUP FILE
;; REQUIREMENT: var     `section-annoyances-no-backup-file'
(when section-annoyances-no-backup-file (message "  10.3 No backup file...")
  ;; Turn off backup files.
  (setq make-backup-files nil)
  (message "  10.3 No backup file... Done"))

;;
;;;; ALL BACKUP FILE IN DIRECTORY
;; REQUIREMENT: var     `section-annoyances-backup-file-in-directory'
(when section-annoyances-backup-file-in-directory (message "  10.4 All backup files in a directory...")
  ;; All backup files goes in a directory.
  (if profile-backup-directory
    (progn
      (setq backup-directory-alist
        `((".*" . ,profile-backup-directory)))
      (setq auto-save-file-name-transforms
        `((".*" ,profile-autosave-directory t)))
      ;;;; no more #foo.bar# files (auto-save file)
      ;;;; I don't want it
      ;;(auto-save-mode nil)
      ) ; (progn
    (progn
      ;; put all backup file in the temporary directory of the system
      (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
      ;; put all auto-save file in the temporary directory of the system
      (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
      ) ; (progn
    ) ; (if profile-backup-directory

  (setq backup-by-copying t   ; don't clobber symlinks
    version-control t         ; use version backups
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2)
  (message "  10.4 All backup files in a directory... Done"))

;;
;;; CLASSIC SCROLL
;; REQUIREMENT: var     `section-annoyances-classic-scroll'
(when section-annoyances-classic-scroll (message "  10.5 Scroll line by line...")
  ;; set with max value
  (setq scroll-conservatively most-positive-fixnum)
  (message "  10.5 Scroll line by line... Done"))

(provide 'annoyances)

;;; annoyances.el ends here
