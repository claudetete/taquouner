;;; shortcut-ecb.el --- a config file for shortcut of ecb mode

;; Copyright (c) 2010-2013 Claude Tete
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

;; Keywords: config, ecb, mode, shortcut
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.8
;; Created: August 2010
;; Last-Updated: March 2013

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-ecb'
;;              var     `section-shortcut'
;;              var     `section-mode-cedet-ecb'

;;; Change Log:
;; 2013-03-26 (1.8)
;;    fix bug with ecb directories window and f2 shortcut
;; 2013-01-18 (1.7)
;;    new shortcut to open in dired from dir window
;; 2012-06-20 (1.6)
;;    add smart shortcuts in ecb window
;; 2012-05-10 (1.5)
;;    add go back with M-*
;; 2012-04-17 (1.4)
;     add condition about section ecb mode + remove C-c qaz shortcut
;; 2012-03-30 (1.3)
;;    translate comments in English + cleaning
;; 2012-03-23 (1.2)
;;    add shortcut for methods & source
;; 2011-07-21 (1.1)
;;    add shortcut for methods & source
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2010-08-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
(when section-mode-cedet-ecb
  ;; hide/show ecb window
  ;;;; already used for quake-like terminal
  ;;(global-set-key       [f1]                    'ecb-toggle-ecb-windows)
  (global-set-key       (kbd "M-1")             'ecb-toggle-ecb-windows)

  ;;
  ;; hide/show ecb compile window
  (global-set-key       (kbd "<f2>")            'ecb-toggle-compile)
  (global-set-key       (kbd "<mouse-5>")       'ecb-toggle-compile-window)

  ;;
  ;; increase/decrease width of ecb window
  (global-set-key       "\C-cw"                 'ecb-toggle-width)

  ;;
  ;; got to the ecb directory window
  (global-set-key       "\M-q"                  'ecb-open-directories)
  ;;
  ;; got to the ecb source window
  (global-set-key       "\M-a"                  'ecb-open-sources)
  ;;
  ;; got to the ecb method window (function/variable...)
  (global-set-key       "\M-\\"                 'ecb-open-methods)
  ;;
  ;; got to the ecb history window (all opened file, not all buffers)
  (global-set-key       "\M-z"                  'ecb-open-history)

  ;; go back thanks to ecb
  (global-set-key       (kbd "<M-kp-multiply>") 'ecb-nav-goto-previous)

  ;; after create directories buffer
  (add-hook 'ecb-directories-buffer-after-create-hook
    '(lambda ()
       ;; remap enter only in ecb directories buffer
       (local-set-key   (kbd "<return>")        'ecb-directories-select)
       (local-set-key   (kbd "<M-return>")      'ecb-dired-directory)
       (local-set-key   (kbd "<escape>")        'ecb-cancel)
       (local-set-key   (kbd "M-q")             'ecb-toggle-maximize)
       ;; hide/show ecb compile window
       (local-set-key   (kbd "<f2>")            'ecb-toggle-compile)
       ))

  ;; after create sources buffer
  (add-hook 'ecb-sources-buffer-after-create-hook
    '(lambda ()
       ;; remap enter only in ecb sources buffer
       (local-set-key   (kbd "<return>")        'ecb-select)
       (local-set-key   (kbd "<M-return>")      'ecb-sources-select)
       (local-set-key   (kbd "<escape>")        'ecb-cancel)
       (local-set-key   (kbd "M-a")             'ecb-toggle-maximize)
       ))

  ;; after create methods buffer
  (add-hook 'ecb-methods-buffer-after-create-hook
    '(lambda ()
       ;; remap enter only in ecb methods buffer
       (local-set-key   (kbd "<return>")        'ecb-select)
       (local-set-key   (kbd "<escape>")        'ecb-cancel)
       (local-set-key   (kbd "M-\\")            'ecb-toggle-maximize)
       ))

  ;; after create history buffer
  (add-hook 'ecb-history-buffer-after-create-hook
    '(lambda ()
       ;; remap enter only in ecb history buffer
       (local-set-key   (kbd "<return>")        'ecb-select)
       (local-set-key   (kbd "<escape>")        'ecb-cancel)
       (local-set-key   (kbd "M-z")             'ecb-toggle-maximize)
       ))

) ; when section-mode-cedet-ecb


(provide 'shortcut-ecb)

;;; shortcut-ecb.el ends here
