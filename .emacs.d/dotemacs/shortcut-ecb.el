;;; shortcut-ecb.el --- a config file for shortcut of ecb mode

;; Copyright (c) 2010, 2011, 2012 Claude Tete
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
;; Version: 1.3
;; Created: August 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-ecb'
;;              var     `section-shortcut'
;;              var     `section-mode-cedet-ecb'

;;; Change Log:
;; 2012-04-17 (1.4)
;     add condition about section ecb mode + remove C-c qaz shortcut
;; 2012-03-30 (1.3)
;;    translate comments in english + cleaning
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
  ;;(global-set-key         [f1]                    'ecb-toggle-ecb-windows)
  (global-set-key         "\C-c\\"                'ecb-toggle-ecb-windows)

  ;;
  ;; hide/show ecb compil window
  (global-set-key         [f2]                    'ecb-toggle-compile-window)
  (global-set-key         (kbd "<mouse-5>")       'ecb-toggle-compile-window)

  ;;
  ;; increase/decrease width of ecb window
  (global-set-key         "\C-cw"                 'ecb-toggle-width)

  ;;
  ;; got to the ecb directory window
;;  (global-set-key         "\C-cq"                 'ecb-myopen-directories)
  (global-set-key         "\M-q"                  'ecb-myopen-directories)
  ;;
  ;; got to the ecb source window
  ;;(global-set-key         "\C-ca"                 'ecb-myopen-sources)
  (global-set-key         "\M-a"                  'ecb-myopen-sources)
  ;;
  ;; got to the ecb method window (function/variable...)
  ;;(global-set-key         "\C-cz"                 'ecb-myopen-methods)
  (global-set-key         "\M-\\"                 'ecb-myopen-methods)
  ;;
  ;; got to the ecb history window (all opened file, not all buffers)
  (global-set-key         "\M-z"                  'ecb-goto-window-history)
) ; when section-mode-cedet-ecb

;;; shortcut-ecb.el ends here
