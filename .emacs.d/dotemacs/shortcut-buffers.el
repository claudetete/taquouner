;;; shortcut-buffers.el --- a config file for buffer shortcut

;; Copyright (c) 2006-2013 Claude Tete
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

;; Keywords: config, shortcut, buffer
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: February 2013

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-buffers'
;;              var     `section-shortcut'

;;; Change Log:
;; 2013-02-05 (1.9)
;;    add shortcut for auctex mode
;; 2012-12-27 (1.8)
;;    switch shortcut for special buffer + toggle compile window when bookmark
;;    jump
;; 2012-11-30 (1.7)
;;    add shortcut for special buffer like grep or vc-diff
;; 2012-08-01 (1.6)
;;    add hide ecb compile window with quiting diff and log
;; 2012-07-09 (1.5)
;;    add shortcut to quit diff mode
;; 2012-06-21 (1.4)
;;    remove old shortcut to switch between buffers
;; 2012-06-08 (1.3)
;;    add clearcase shortcuts
;; 2012-03-30 (1.2)
;;    translate comments in English + cleaning
;; 2012-03-23 (1.1)
;;    change kill this buffer shortcut to have logical ecb shortcut
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; close the current buffer
(global-set-key         (kbd "M-`")             'kill-this-buffer)
;;
;; show a list of buffers in a new window
(global-set-key         (kbd "C-x C-b")         'electric-buffer-list)
;;
;; show the window of bookmark
(global-set-key         "\C-cb"                 'bookmark-bmenu-list)

;;;; go to the next buffer (like new editor which I never like it)
;;(global-set-key         [(control tab)]         'previous-user-buffer)
;;;;
;;;; go to the previous buffer (like new editor which I never like it)
;;(global-set-key         [(control backtab)]     'next-user-buffer)

;; switch to grep or ack buffer
(global-set-key         (kbd "M-2")             'switch-to-grep-ack-buffer)
;; switch to compile buffer
(global-set-key         (kbd "M-3")             'switch-to-bookmark-buffer)
;; switch to occur buffer
(global-set-key         (kbd "M-4")             'switch-to-compilation-buffer)
;; switch to vc buffer
(global-set-key         (kbd "M-5")             'switch-to-vc-buffer)
;; switch to help buffer
(global-set-key         (kbd "M-6")             'switch-to-help-buffer)
;; the previous global-set-key are unset in diff mode ???
(add-hook 'diff-mode-hook
  (lambda ()
    ;; switch to grep or ack buffer
    (local-set-key      (kbd "M-2")             'switch-to-grep-ack-buffer)
    ;; switch to compile buffer
    (local-set-key      (kbd "M-3")             'switch-to-bookmark-buffer)
    ;; switch to occur buffer
    (local-set-key      (kbd "M-4")             'switch-to-compilation-buffer)
    ;; switch to vc buffer
    (local-set-key      (kbd "M-5")             'switch-to-vc-buffer)
    ;; switch to help buffer
    (local-set-key      (kbd "M-6")             'switch-to-help-buffer)
    ))

;; only for clearcase mode
(when section-mode-clearcase
  ;; ediff with previous (can open a new frame before with C-x 5 2)
  (global-set-key       (kbd "C-x v `")         'clearcase-ediff-pred-current-buffer)
  ;; ediff with named version
  (global-set-key       (kbd "C-x v 1")         'clearcase-ediff-named-version-current-buffer)
  ) ; (when section-mode-clearcase

;; only for subversion mode
(when section-mode-subversion
  (if section-mode-cedet-ecb
    (progn
      ;; make 'q' quit buffer and close window
      ;; in diff mode
      (add-hook 'svn-status-diff-mode-hook
        (lambda ()
          (define-key svn-status-diff-mode-map      "q"     '(lambda ()
                                                               (interactive)
                                                               (delete-window)
                                                               (ecb-toggle-compile)))))
      ;; in log mode
      (add-hook 'svn-log-view-mode-hook
        (lambda ()
          (define-key svn-log-view-mode-map         "q"     '(lambda ()
                                                               (interactive)
                                                               (delete-window)
                                                               (ecb-toggle-compile)))))
      ) ; (progn
    (progn
      ;; make 'q' quit buffer and close window
      ;; in diff mode
      (add-hook 'svn-status-diff-mode-hook
        (lambda ()
          (define-key svn-status-diff-mode-map      "q"     'delete-window)))
      ;; in log mode
      (add-hook 'svn-log-view-mode-hook
        (lambda ()
          (define-key svn-log-view-mode-map         "q"     'delete-window)))
      ) ; (progn
    ) ; (if section-mode-cedet-ecb
  ) ; (when section-mode-subversion


;;
(when section-mode-auctex
  (add-hook 'LaTeX-mode-hook '
    (lambda ()
      (local-set-key    (kbd "<f10>")           'auctex-save-compile-and-view)
      )))


(provide 'shortcut-buffers)

;;; shortcut-buffers.el ends here
