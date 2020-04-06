;;; 08-shortcut-01-windows.el --- a config file for windows shortcut

;; Copyright (c) 2006-2019 Claude Tete
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;; Author: Claude Tete <claude.tete@gmail.com>
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.add shortcut to manage windows]
;; [SUBDEFAULT.t]


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
