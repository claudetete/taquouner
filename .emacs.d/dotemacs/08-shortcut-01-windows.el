;;; 08-shortcut-01-windows.el --- a config file for windows shortcut

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
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage windows]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-26 (1.9)
;;    update to new conf format
;; 2017-05-26 (1.8)
;;    add new shortcut to delete frame (cannot remember previous one)
;; 2013-09-10 (1.7)
;;    add shortcut to delete window or frame
;; 2013-04-10 (1.6)
;;    bind myself windmove to add ignore error
;; 2012-08-01 (1.5)
;;    use smart resize from functions.el
;; 2012-07-09 (1.4)
;;    add shortcut to split windows more easily
;; 2012-06-21 (1.3)
;;    additional shortcut to split windows in Emacs
;; 2012-04-17 (1.2)
;;    remove M-s and M-arrow to switch between window
;; 2012-03-30 (1.1)
;;    split .emacs file
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; use default binding to move between window
;; S-arrow (add ignore-error from `windmove-default-keybindings')
(global-set-key         (kbd "<S-left>")        (lambda ()
                                                  (interactive)
                                                  (ignore-errors (windmove-left))))
(global-set-key         (kbd "<S-right>")       (lambda ()
                                                  (interactive)
                                                  (ignore-errors (windmove-right))))
(global-set-key         (kbd "<S-up>")          (lambda ()
                                                  (interactive)
                                                  (ignore-errors (windmove-up))))
(global-set-key         (kbd "<S-down>")        (lambda ()
                                                  (interactive)
                                                  (ignore-errors (windmove-down))))

;; new shortcut for split windows
(global-set-key         (kbd "<M-kp-decimal>")  'delete-window)
(global-set-key         (kbd "<M-kp-0>")        'delete-other-windows)
(global-set-key         (kbd "<M-kp-2>")        (lambda ()
                                                  (interactive)
                                                  (split-window-below)
                                                  (windmove-down 0)))
(global-set-key         (kbd "<C-kp-2>")        'delete-window)
(global-set-key         (kbd "<M-kp-6>")        (lambda ()
                                                  (interactive)
                                                  (split-window-right)
                                                  (windmove-right 0)))
(global-set-key         (kbd "<C-kp-6>")        'delete-window)
(global-set-key         (kbd "<M-kp-5>")        'make-frame-command)
(global-set-key         (kbd "<M-S-kp-5>")      'delete-frame)
(global-set-key         (kbd "<C-kp-5>")        'delete-frame)
(global-set-key         (kbd "<C-kp-decimal>")  'delete-frame)


(provide '08-shortcut-01-windows)

;;; 08-shortcut-01-windows.el ends here
