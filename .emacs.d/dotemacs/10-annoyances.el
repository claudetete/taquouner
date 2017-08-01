;;; 10-annoyances.el --- a config file for all annoying things

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
;; Version: 2.1
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [[HEADER.no welcome message + yes->y + do not query to refresh buffer + remove insert
;; key + remove C-Pup & C-Dwn + wheel click do nothing + no dialog box + no
;; tooltips
;; ]]

;;; Change Log:
;; 2017-07-26 (2.1)
;;    update to new conf format
;; 2013-09-10 (2.0)
;;    remove old code
;; 2012-07-09 (1.9)
;;    scroll horizontally when no truncate line
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
;; [SUBCOMMENT.TRUNCATE LINE: whole line not visible (need to scroll right)]
;; [SUBSECTION.tqnr-section-annoyances-truncate-line nil]
(when tqnr-section-annoyances-truncate-line (message "    Truncate Line...")
  ;; do not truncate line
  ;; a displayed row is a line but can be hidden outside window (need to
  ;; scroll horizontally)
  (setq-default truncate-lines nil)

  ;; do scroll horizontally when at edge of screen
  (setq auto-hscroll-mode t)
  (setq hscroll-margin 0)
  (message "    Truncate Line... Done"))

;; [COMMENT.]
;; [SUBCOMMENT.SCROLL PRESERVE CURSOR POSITION: when wheel scroll the cursor do not move]
;; [SUBSECTION.tqnr-section-annoyances-scroll-preserve-cursor-position nil]
(when tqnr-section-annoyances-scroll-preserve-cursor-position (message "    Scroll preserve cursor position...")
  ;; return to same line on a scroll back
  (setq scroll-preserve-screen-position t)
  (message "    Scroll preserve cursor position... Done"))

;; [COMMENT.]
;; [SUBCOMMENT.NO BACKUP FILE: turn off backup files]
;; [SUBSECTION.tqnr-section-annoyances-no-backup-file nil]
(when tqnr-section-annoyances-no-backup-file (message "    No backup file...")
  ;; Turn off backup files
  (setq make-backup-files nil)
  (message "    No backup file... Done"))

;; [COMMENT.]
;; [SUBCOMMENT.ALL BACKUP FILE IN DIRECTORY: all backup files goes in a directory]
;; [SUBSECTION.tqnr-section-annoyances-backup-file-in-directory nil]
(when tqnr-section-annoyances-backup-file-in-directory (message "    All backup files in a directory...")
  ;; [VARCOMMENT.all backup files goes in a directory]
  ;; [VARIABLE.tqnr-profile-backup-directory nil]
  ;; [VARIABLE.tqnr-profile-autosave-directory nil]
  (if tqnr-profile-backup-directory
    (progn
      (setq backup-directory-alist
        `((".*" . ,tqnr-profile-backup-directory)))
      (setq auto-save-file-name-transforms
        `((".*" ,tqnr-profile-autosave-directory t)))
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
    ) ; (if tqnr-profile-backup-directory

  (setq backup-by-copying t   ; don't clobber symlinks
    version-control t         ; use version backups
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2)
  (message "    All backup files in a directory... Done"))


(provide '10-annoyances)

;;; 10-annoyances.el ends here
