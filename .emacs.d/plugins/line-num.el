;;; Line-num.el --- Display line numbers in left-margin of buffer.
;; 
;; Emacs Lisp Archive Entry
;; Filename: line-num.el
;; Description: Display line numbers in left-margin of buffer.
;; Author: (Darryl Okahata) darrylo@hpsrdmo
;; Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2001, Drew Adams, all rights reserved.
;; Copyright (C) 1989, Hewlett-Packard, all rights reserved. 
;; Created: Wed Mar 31 16:18:24 1999
;; Version: $Id: line-num.el,v 1.7 2001/01/08 23:24:38 dadams Exp $
;; Last-Updated: Mon Jan  8 15:24:19 2001
;;           By: dadams
;;     Update #: 156
;; Keywords: local
;; Compatibility: GNU Emacs 20.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Display line numbers in left-margin of buffer.
;;
;; New functions defined here:
;;
;;    `display-line-numbers', `toggle-line-numbers-display',
;;    `turn-on-line-numbers-display', `turn-off-line-numbers-display'.
;;
;; NOTE: `setnu.el' now provides similar, but generally better,
;; functionality.
;;
;; Original author was Darryl Okahata darrylo@hpsrdmo: The copy on
;; which the current (Adams) modifications were made was obtained from
;; Rick Kunin (rickk@sperdk).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: line-num.el,v $
;; RCS Revision 1.7  2001/01/08 23:24:38  dadams
;; RCS Adapted file header for Emacs Lisp Archive.
;; RCS
;; RCS Revision 1.6  2001/01/08 19:41:52  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.5  2001/01/03 17:39:23  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.4  2001/01/03 00:48:02  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.3  2000/12/07 19:50:10  dadams
;; RCS Added require of shrink-fit.el.
;; RCS
;; RCS Revision 1.2  2000/11/01 16:05:30  dadams
;; RCS 1. Added: toggle-line-numbers-display, turn-on-line-numbers-display,
;; RCS           turn-off-line-numbers-display.
;; RCS 2. Require cl.el.
;; RCS 3. Provide line-num.
;; RCS 4. Added global vars: displaying-line-numbers-p,
;; RCS    display-line-numbers-format-string, display-line-numbers-first-line,
;; RCS    display-line-numbers-count, display-line-numbers-buffer-name,
;; RCS    display-line-numbers-modified-p.
;; RCS
;; RCS Revision 1.1  2000/09/14 17:20:43  dadams
;; RCS Initial revision
;; RCS
; Revision 1.4  1999/04/14  08:31:17  dadams
; Commented out assignment to unused, free var: insert-end.
;
; Revision 1.3  1999/04/13  14:01:18  dadams
; *** empty log message ***
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;>> Problem:  Tabs at beginning of lines


(require 'cl) ;; when

(require 'shrink-fit nil t) ;; (no error if not found): shrink-frame-to-fit


(provide 'line-num)

;;;;;;;;;;;;;


;;;###autoload
(defun display-line-numbers ()
  "Temporarily display line numbers in left margin of current buffer."
  (interactive)
  (or (eq (current-buffer) (window-buffer (selected-window)))
      (error "Current buffer, %s, is not the selected window's buffer."
             (buffer-name)))
  (let ((buffer-read-only nil)
        (modified (buffer-modified-p))
        (name buffer-file-name)
        (point (point-marker))
        format-string
        line-number
        (count 0)
        nlines
        first-line)
    (save-restriction
      (widen)
      (save-excursion
        (setq first-line (window-start (selected-window)))
        (goto-char first-line)
        (setq line-number (1+ (count-lines (point-min) (point))))
        (move-to-window-line -1)
        (beginning-of-line)
        (setq nlines (count-lines first-line (point)))
        (let ((max (+ line-number nlines)))
          (setq format-string (cond ((< max 100) "%2d ")
                                    ((< max 1000) "%3d ")
                                    ((< max 10000) "%4d ")
                                    (t "%7d "))))))
    (save-excursion
      (unwind-protect
          (progn
            (goto-char first-line)
            ;; defeat file locking... don't try this at home, kids!
            (setq buffer-file-name nil)
            (while (<= count nlines)
              (insert-before-markers (format format-string line-number))
              ;;;(setq insert-end (point)) THIS VAR IS FREE - AND UNUSED!
              (setq line-number (1+ line-number))
              (setq count (1+ count))
              (forward-line 1))
            (set-window-start (selected-window) first-line)
            (goto-char point)
            (set-buffer-modified-p modified)
            (message "<<< Press SPACE to continue >>>")
            (let ((char (read-char)))
              (or (eql char ?\  )
                  (setq unread-command-events (list char))))
            )
        (goto-char first-line)
        (let ((n (1+ (- (aref format-string 1) ?0))))
          (while (> count 0)
            (setq count (1- count))
            (delete-char n)
            (forward-line 1)))
        (setq buffer-file-name name)
        (set-buffer-modified-p modified)))))

;;;-----------------------------------------------------------------

(defvar displaying-line-numbers-p nil)
(make-variable-buffer-local 'displaying-line-numbers-p)
(defvar display-line-numbers-format-string)
(make-variable-buffer-local 'display-line-numbers-format-string)
(defvar display-line-numbers-first-line)
(make-variable-buffer-local 'display-line-numbers-first-line)
(defvar display-line-numbers-count 0)
(make-variable-buffer-local 'display-line-numbers-count)
(defvar display-line-numbers-buffer-name)
(make-variable-buffer-local 'display-line-numbers-buffer-name)
(defvar display-line-numbers-modified-p)
(make-variable-buffer-local 'display-line-numbers-modified-p)

;;;###autoload
(defun toggle-line-numbers-display (window-only-p)
  "Display/clear line numbers in left margin of current buffer.
With prefix arg, just number lines in current window, not all lines in
buffer."
  (interactive "P")
  (if displaying-line-numbers-p
      (turn-off-line-numbers-display)
    (turn-on-line-numbers-display window-only-p)))

;;;###autoload
(defun turn-on-line-numbers-display (window-only-p)
  "Display line numbers in left margin of current buffer.
With prefix arg, just number lines in current window, not all lines in
buffer."
  (interactive "P")
  (or (eq (current-buffer) (window-buffer (selected-window)))
      (error "Current buffer, %s, is not the selected window's buffer."
             (buffer-name)))
  (let ((buffer-read-only nil)
        (point (point-marker))
        line-number
        nlines)
    (setq display-line-numbers-buffer-name buffer-file-name)
    (setq display-line-numbers-modified-p (buffer-modified-p))
    (save-restriction
      (widen)
      (save-excursion
        (setq display-line-numbers-first-line
              (if window-only-p
                  (window-start (selected-window))
                (point-min)))
        (goto-char display-line-numbers-first-line)
        (setq line-number (1+ (count-lines (point-min) (point))))
        (if window-only-p
            (move-to-window-line -1)
          (goto-char (point-max)))
        (beginning-of-line)
        (setq nlines (count-lines display-line-numbers-first-line (point)))
        (let ((max (+ line-number nlines)))
          (setq display-line-numbers-format-string (cond ((< max 100) "%2d ")
                                    ((< max 1000) "%3d ")
                                    ((< max 10000) "%4d ")
                                    (t "%7d "))))))
    (save-excursion
      (condition-case nil
          (progn
            (goto-char display-line-numbers-first-line)
            (setq buffer-file-name nil) ; To prevent saving with line numbers etc.
            (setq displaying-line-numbers-p t)
            (while (<= display-line-numbers-count nlines)
              (insert-before-markers
               (format display-line-numbers-format-string line-number))
              (setq line-number (1+ line-number))
              (setq display-line-numbers-count (1+ display-line-numbers-count))
              (forward-line 1))
            (when window-only-p
              (set-window-start (selected-window) display-line-numbers-first-line))
            (goto-char point)
            (set-buffer-modified-p display-line-numbers-modified-p))
        (error
         (progn
           (goto-char display-line-numbers-first-line)
           (let ((n (1+ (- (aref display-line-numbers-format-string 1) ?0))))
             (while (> display-line-numbers-count 0)
               (setq display-line-numbers-count (1- display-line-numbers-count))
               (delete-char n)
               (forward-line 1)))
           (setq buffer-file-name display-line-numbers-buffer-name)
           (set-buffer-modified-p display-line-numbers-modified-p)
           (setq displaying-line-numbers-p nil))))))
  (let ((curr-line (count-lines (window-start) (point))))
    (when (> curr-line 0) (decf curr-line))
    (recenter curr-line))
  (when (fboundp 'shrink-frame-to-fit) (shrink-frame-to-fit)))

;;;###autoload
(defun turn-off-line-numbers-display ()
  "Clear displayed line numbers from left margin of current buffer."
  (interactive)
  (or (eq (current-buffer) (window-buffer (selected-window)))
      (error "Current buffer, %s, is not the selected window's buffer."
             (buffer-name)))
  (let ((buffer-read-only nil))
    (save-excursion
      (when (boundp 'display-line-numbers-buffer-name)
        (setq buffer-file-name display-line-numbers-buffer-name))
      (goto-char display-line-numbers-first-line) 
      (let ((n (1+ (- (aref display-line-numbers-format-string 1) ?0)))) 
        (while (> display-line-numbers-count 0)
          (setq display-line-numbers-count (1- display-line-numbers-count))
          (delete-char n)
          (forward-line 1)))
      (when (boundp 'display-line-numbers-modified-p)
        (set-buffer-modified-p display-line-numbers-modified-p))
      (setq displaying-line-numbers-p nil))
    (when (fboundp 'shrink-frame-to-fit) (shrink-frame-to-fit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `line-num.el' ends here