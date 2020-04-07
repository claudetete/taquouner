;;; 02-mode-038-powerline.el --- configuration of powerline mode

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.fancy modeline]
;; [SUBDEFAULT.t]
;;
;; TODO: use updated package


;;; Code:
(when tqnr-running-on-emacs-23
  ;; in Emacs 23 it was not define
  (defun get-scroll-bar-mode () scroll-bar-mode)
  (defsetf get-scroll-bar-mode set-scroll-bar-mode))

;; new in emacs 24, using sRGB colorspace by default, powerline does not like it
;; see https://github.com/milkypostman/powerline/issues/54
(when (or tqnr-running-on-emacs-24 tqnr-running-on-emacs-25)
  (setq ns-use-srgb-colorspace nil))

;; use new powerline mode
;; see
(use-package powerline
  :config
  (which-function-mode)
  (defun powerline-my-theme ()
    "Setup a mode-line."
    (interactive)
    (setq-default mode-line-format
      '("%e"
         (:eval
           (let* ((active (powerline-selected-window-active))
                   ;; (face-mode-line (if active 'mode-line
                   ;;                   'mode-line-inactive))
                   ;; (mode-line-buffer-id (if active 'mode-line-buffer-id
                   ;;                        'mode-line-buffer-id-inactive))
                   ;; face for right and left end
                   (face-end (if active 'powerline-active0
                               'powerline-inactive0))
                   ;; face for between right and left and middle
                   (face-between (if active 'powerline-active1
                                   'powerline-inactive1))
                   ;; face for middle
                   (face-middle (if active 'powerline-active2
                                  'powerline-inactive2))
                   ;; face for highlight
                   (face-warning `(:height 1.5
                                    :foreground ,(face-foreground (if active 'powerline-highlight-active0 'powerline-highlight-inactive0))
                                    :background ,(face-background (if active 'powerline-highlight-active0 'powerline-highlight-inactive0))))
                   (face-function `(:foreground ,(face-foreground (if active 'font-lock-function-name-face 'powerline-inactive0))
                                     :background ,(face-background (if active 'powerline-active2 'powerline-inactive2))))
                   (face-project `(:foreground ,(face-background (if active 'powerline-active0 'powerline-inactive0))
                                     :background ,(face-background (if active 'powerline-active1 'powerline-inactive1))))
                   (separator-left
                     (intern (format "powerline-%s-%s"
                               (powerline-current-separator)
                               (car powerline-default-separator-dir))))
                   (separator-right
                     (intern (format "powerline-%s-%s"
                               (powerline-current-separator)
                               (cdr powerline-default-separator-dir))))

                   (lhs (list
                          ;;
                          ;; LEFT
                          ;; display [RO] when visited a read-only file
                          (when (and buffer-read-only buffer-file-name)
                            (if tqnr-section-mode-all-the-icons
                              (concat
                                (powerline-raw " " face-end)
                                (all-the-icons-octicon "lock" :face 'font-lock-warning-face :v-adjust 0.05))
                              (powerline-raw "" face-warning)))
                          ;; buffername
                          (powerline-buffer-id face-end 'l)
                          ;; display * at end of buffer name when buffer was modified
                          (when (and (buffer-modified-p) buffer-file-name)
                            (powerline-raw "" face-warning 'l))

                          ;; first separator
                          (powerline-raw " " face-end)
                          (funcall separator-left face-end face-between)

                          ;;
                          ;; LEFT MIDDLE
                          ;; major mode
                          (if tqnr-section-mode-all-the-icons
                            (concat (powerline-raw " " face-between)
                              (if active
                                (all-the-icons-icon-for-mode major-mode :face 'powerline-active1 :v-adjust 0.05)
                                (all-the-icons-icon-for-mode major-mode :face 'powerline-inactive1 :v-adjust 0.05)))
                            (powerline-major-mode face-between 'l))
                          ;; process
                          (powerline-process face-between)
                          ;; minor mode
                          ;;(powerline-minor-modes face-between 'l)
                          (powerline-raw " [" face-between)
                          (powerline-raw (projectile-project-name) face-project)
                          (powerline-raw "]" face-between)
                          ;; narrow mode
                          (powerline-narrow face-between 'l)
                          ))

                   (rhs (list


                          ;;
                          ;; RIGHT MIDDLE
                          ;; line number
                          (powerline-raw "%2l" face-between 'l)
                          ;; :
                          (powerline-raw ":" face-between 'l)
                          ;; column number
                          (powerline-raw "%2c" face-between 'r)

                          ;; fourth separator
                          (funcall separator-right face-between face-end)
                          (powerline-raw " " face-end)

                          ;;
                          ;; RIGHT
                          ;; encoding and eol indicator
                          (when buffer-file-coding-system
                            (powerline-raw
                              (let* ((eol (coding-system-eol-type buffer-file-coding-system)))
                                (cond
                                  ((eq eol 0) "\\n ")
                                  ((eq eol 1) "\\r\\n ")
                                  ((eq eol 2) "\\r ")
                                  (t ""))) face-end))
                          ;; position indicator
                          (powerline-raw "%6p" face-end 'r)

                          ))

                   (center
                     (list
                       ;; second separator
                       (funcall separator-left face-between face-middle)
                       (powerline-raw " " face-middle)

                       ;;
                       ;; MIDDLE
                       (powerline-raw (if (eq major-mode 'ada-mode)
                                        (ada-which-function)
                                        (which-function))
                        face-function)

                       ;; third separator
                       (powerline-raw global-mode-string face-middle 'r)
                       (powerline-raw " " face-middle)
                       (funcall separator-right face-middle face-between)
                       )))
             ;;(message "%s %s" separator-left (funcall 'powerline-wave-left mode-line face1))
             (concat
               (powerline-render lhs)
               (powerline-fill-center face-between (/ (powerline-width center) 2.0))
               (powerline-render center)
               (powerline-fill face-between (powerline-width rhs))
               (powerline-render rhs)))))))

  ;; set arrow fade as separator
  (setq powerline-default-separator 'arrow)
  (setq powerline-height 24)
  (powerline-my-theme)
  ) ;; (use-package powerline


(provide '02-mode-038-powerline)

;;; 02-mode-038-powerline.el ends here
