;;; interface-modeline.el --- a config file for Emacs modeline

;; Copyright (c) 2012 Claude Tete
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

;; Keywords: config, interface
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.1
;; Created: April 2012
;; Last-Updated: April 2012

;;; Commentary:
;;
;; load by `interface.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-interface-modeline'
;;
;; TODO: put all option like now but with color

;;; Change Log:
;; 2012-04-03 (0.1)
;;    creation from emacs-fu blog + current value


;;; Code:
(defvar section-interface-modeline-old t)

(if section-interface-modeline-old
  (progn
    ;; show column number in modeline
    (column-number-mode t)

    ;; show time in 24H format (example 23:59)
    (setq display-time-24hr-format t)
    ;;
    ;; show time and date in modeline
    (setq display-time-day-and-date t)

    ;; show date in european format (example: jeu. 29 mars)
    (custom-set-variables
      '(display-time-string-forms
         (quote
           ((if
              (and
                (not display-time-format)
                display-time-day-and-date)
              (format-time-string "%a %e %b " now) "")
             (propertize
               (format-time-string
                 (or display-time-format
                   (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                 now)
               (quote help-echo)
               (format-time-string "%a %e %b, %Y" now))
             load
             (if mail
               (concat " "
                 (propertize display-time-mail-string (quote display)
                   (\` (when
                         (and display-time-use-mail-icon
                           (display-graphic-p))
                         (\,@ display-time-mail-icon)
                         (\,@ (if
                                (and display-time-mail-face
                                  (memq
                                    (plist-get
                                      (cdr display-time-mail-icon)
                                      :type)
                                    (quote (pbm xbm))))
                                (let
                                  ((bg
                                     (face-attribute display-time-mail-face
                                       :background)))
                                  (if
                                    (stringp bg)
                                    (list :background bg)))))))
                   (quote face) display-time-mail-face (quote help-echo) "You have new mail; mouse-2: Read mail" (quote mouse-face) (quote mode-line-highlight) (quote local-map)
                   (make-mode-line-mouse-map (quote mouse-2) read-mail-command)))
               ""))))
      )
    ;;
    ;; show time in modeline
    (display-time-mode t)

    ;; display size of file in the modeline
    (size-indication-mode t)
    ) ; progn

  ;; else section-interface-modeline-old
  (progn
    ;; use setq-default to set it for /all/ modes
    (setq mode-line-format
      (list
        ;; the buffer name; the file name as a tool tip
        '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                  'help-echo (buffer-file-name)))

        ;; line and column
        "(" ;; '%02' to set to 2 chars at least; prevents flickering
        (propertize "%02l" 'face 'font-lock-type-face) ","
        (propertize "%02c" 'face 'font-lock-type-face)
        ") "

        ;; relative position, size of file
        "["
        (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
        "/"
        (propertize "%I" 'face 'font-lock-constant-face) ;; size
        "] "

        ;; the current major mode for the buffer.
        "["

        '(:eval (propertize "%m" 'face 'font-lock-string-face
                  'help-echo buffer-file-coding-system))
        "] "


        "[" ;; insert vs overwrite mode, input-method in a tooltip
        '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                  'face 'font-lock-preprocessor-face
                  'help-echo (concat "Buffer is in "
                               (if overwrite-mode "overwrite" "insert") " mode")))

        ;; was this buffer modified since the last save?
        '(:eval (when (buffer-modified-p)
                  (concat ","  (propertize "Mod"
                                 'face 'font-lock-warning-face
                                 'help-echo "Buffer has been modified"))))

        ;; is this buffer read-only?
        '(:eval (when buffer-read-only
                  (concat ","  (propertize "RO"
                                 'face 'font-lock-type-face
                                 'help-echo "Buffer is read-only"))))
        "] "

        ;; add the time, with the date and the emacs uptime in the tooltip
        '(:eval (propertize (format-time-string "%H:%M")
                  'help-echo
                  (concat (format-time-string "%c; ")
                    (emacs-uptime "Uptime:%hh"))))
        " --"
        ;; i don't want to see minor-modes; but if you want, uncomment this:
        ;;minor-mode-alist  ;; list of minor modes
        "%-" ;; fill with '-'
        ))


;;;; Current value
    (setq mode-line-format
      ("%e"
        #("-" 0 1
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
        mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
        #("   " 0 3
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
        mode-line-position
        (vc-mode vc-mode)
        #("  " 0 2
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
        mode-line-modes
        (which-func-mode
          ("" which-func-format
            #("--" 0 2
               (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
        (global-mode-string
          ("" global-mode-string
            #("--" 0 2
               (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
        #("-%-" 0 3
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")))
      )

    ) ; progn
  ) ; if
