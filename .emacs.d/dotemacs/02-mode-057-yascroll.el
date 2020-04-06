;;; 02-mode-057-yascroll.el --- configuration of yascroll mode

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [[SUBHEADER.add a small visual scroll-bar (can not be used with mouse click)
;; see https://github.com/m2ym/yascroll-el for screenshot
;; ]]
;; [SUBDEFAULT.t]


;;; Code:
(use-package yascroll
  :init
  (global-yascroll-bar-mode t)

  :config
  ;; (set-face-background 'yascroll:thumb-text-area (face-background (if active 'powerline-active0 'powerline-inactive0)))
  ;; (set-face-background 'yascroll:thumb-fringe (face-background (if active 'powerline-active0 'powerline-inactive0)))
  ;; (set-face-foreground 'yascroll:thumb-fringe (face-background (if active 'powerline-active0 'powerline-inactive0)))
  ;; [VARCOMMENT.time before hide scroll-bar (nil to always show)]
  ;; [VARIABLE.tqnr-profile-yascroll-delay-to-hide nil]
  (setq yascroll:delay-to-hide tqnr-profile-yascroll-delay-to-hide))


(provide '02-mode-057-yascroll)

;;; 02-mode-057-yascroll.el ends here
