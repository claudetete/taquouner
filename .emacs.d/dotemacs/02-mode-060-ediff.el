;;; 02-mode-060-ediff.el --- configuration of ediff mode

;; Copyright (c) 2017 Claude Tete
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
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.graphical diff (## to toggle whitespace ignoring)]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'ediff "    ")
  ;; always split with two vertical buffer in ediff mode
  ;;(add-hook 'ediff-before-setup-hook 'new-frame)
  ;;(add-hook 'ediff-quit-hook 'delete-frame)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))


(add-hook 'ediff-startup-hook
  (lambda ()
    ;; previous diff
    (define-key ediff-mode-map  (kbd "<M-up>")          'ediff-previous-difference)
    ;; next diff
    (define-key ediff-mode-map  (kbd "<M-down>")        'ediff-next-difference)
    ;; get modification from left
    (define-key ediff-mode-map  (kbd "<M-right>")       'ediff-copy-A-to-B)
    ;; get modification from right
    (define-key ediff-mode-map  (kbd "<M-left>")        'ediff-copy-B-to-A)
    )
  )


(provide '02-mode-060-ediff)

;;; 02-mode-060-ediff.el ends here
