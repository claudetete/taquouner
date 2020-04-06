;;; 06-interface-01-modeline.el --- a config file for Emacs Modeline

;; Copyright (c) 2012-2019 Claude Tete
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
;; Version: 0.4
;; Created: April 2012
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [[SUBHEADER.set some option to add in the grey line at the bottom of each buffer
;; (replaced by powerline mode)
;; ]]
;; [SUBDEFAULT.t]
;;
;; TODO: put all option like now but with color


;;; Code:
(if t
  (progn
    ;; show column number in modeline
    (column-number-mode t)

    (when (not tqnr-section-mode-powerline)
      ;; show time in 24H format (example 23:59)
      (setq display-time-24hr-format t)
      ;;
      ;; show time and date in modeline
      (setq display-time-day-and-date t)
      ;;
      ;; show time in modeline
      (display-time-mode t)

      ;; display size of file in the modeline
      (size-indication-mode t)
      ) ; (when (not tqnr-section-mode-powerline)

    ;; show date in European format (example: jeu. 29 mars)
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
    ) ; progn

  ;; else no more used
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

    ;;; from www.hollenback.net
    ;; Set the modeline to tell me the filename, hostname, etc..
    (setq-default mode-line-format
      (list " "
        ;; */% indicators if the file has been modified
        'mode-line-modified
        "--"
        ;; the name of the buffer (i.e. filename)
        ;; note this gets automatically highlighted
        'mode-line-buffer-identification
        "--"
        ;; major and minor modes in effect
        'mode-line-modes
        ;; if which-func-mode is in effect, display which
        ;; function we are currently in.
        '(which-func-mode ("" which-func-format "--"))
        ;; line, column, file %
        'mode-line-position
        "--"
        ;; if vc-mode is in effect, display version control
        ;; info here
        `(vc-mode vc-mode)
        "--"
        ;; hostname
        'system-name
        ;; dashes sufficient to fill rest of modeline.
        "-%-"
        )
      )
      ;;; This results in a modeline like this:
      ;;;  **--.emacs --[(Emacs-Lisp)]--[prefix-region]--28% (201,23) --ourtownadd-lm----------------------

    ) ; progn
  ) ; if


(provide '06-interface-01-modeline)

;; 06-interface-01-modeline.el ends here
