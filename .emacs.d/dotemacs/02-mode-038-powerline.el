;;; 02-mode-038-powerline.el --- configuration of powerline mode

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
;; [SUBHEADER.fancy modeline]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when tqnr-running-on-emacs-23
  ;; in Emacs 23 it was not define
  (defun get-scroll-bar-mode () scroll-bar-mode)
  (defsetf get-scroll-bar-mode set-scroll-bar-mode))

;; use new powerline mode
;; see
(when (try-require 'powerline "    ")
  (defun powerline-my-theme ()
    "Setup a mode-line."
    (interactive)
    (setq-default mode-line-format
      '("%e"
         (:eval
           (let* ((active (powerline-selected-window-active))
                   ;; face for right and left
                   (mode-line (if active 'mode-line 'mode-line-inactive))
                   ;; face for between right and left and middle
                   (face-between (if active 'powerline-active1
                                   'powerline-inactive1))
                   ;; face for middle
                   (face-middle (if active 'powerline-active2
                                  'powerline-inactive2))
                   ;; face for highlight
                   (face-warning 'font-lock-warning-face)
                   (separator-left
                     (intern (format "powerline-%s-%s"
                               powerline-default-separator
                               (car powerline-default-separator-dir))))
                   (separator-right
                     (intern (format "powerline-%s-%s"
                               powerline-default-separator
                               (cdr powerline-default-separator-dir))))
                   (lhs (list
                          ;;
                          ;; LEFT
                          ;; display [RO] when visited a read-only file
                          (when (and buffer-read-only buffer-file-name)
                            (powerline-raw "[RO]" face-warning))
                          ;; encoding and eol indicator
                          (powerline-raw mode-line-mule-info nil 'l)
                          ;; buffername
                          (powerline-buffer-id nil 'l)
                          ;; display * at end of buffer name when buffer was modified
                          (when (and (buffer-modified-p) buffer-file-name)
                            (powerline-raw "*" face-warning 'l))

                          ;; first separator
                          (powerline-raw " ")
                          (funcall separator-left mode-line face-between)

                          ;;
                          ;; LEFT MIDDLE
                          ;; major mode
                          (powerline-major-mode face-between 'l)
                          ;; process
                          (powerline-process face-between)
                          ;; minor mode
                          (powerline-minor-modes face-between 'l)
                          ;; narrow mode
                          (powerline-narrow face-between 'l)

                          ;; second separator
                          (powerline-raw " " face-between)
                          (funcall separator-left face-between face-middle)

                          ;;
                          ;; MIDDLE
                          ;; version control
                          (powerline-vc face-middle 'r)))
                   (rhs (list

                          ;; third separator
                          (powerline-raw global-mode-string face-middle 'r)
                          (funcall separator-right face-middle face-between)

                          ;;
                          ;; RIGHT MIDDLE
                          ;; line number
                          (powerline-raw "%2l" face-between 'l)
                          ;; :
                          (powerline-raw ":" face-between 'l)
                          ;; column number
                          (powerline-raw "%2c" face-between 'r)

                          ;; fourth separator
                          (funcall separator-right face-between mode-line)
                          (powerline-raw " ")

                          ;;
                          ;; RIGHT
                          ;; position indicator
                          (powerline-raw "%6p" nil 'r)
                          )))
             ;;(message "%s %s" separator-left (funcall 'powerline-wave-left mode-line face1))
             (concat
               (powerline-render lhs)
               (powerline-fill face-middle (powerline-width rhs))
               (powerline-render rhs)))))))
  ;; set arrow fade as separator
  (setq powerline-default-separator 'arrow-fade)
  (powerline-my-theme)
  )


(provide '02-mode-038-powerline)

;;; 02-mode-038-powerline.el ends here
