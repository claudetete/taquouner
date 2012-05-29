;;; shortcut-function.el --- a config file for function shortcut

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, shortcut, function
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.9
;; Created: October 2006
;; Last-Updated: May 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-shortcut-function'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-05-29 (1.9)
;;    add shortcuts for new clearcase integration function
;; 2012-05-14 (1.8)
;;    add improve tab key when hide show mode
;; 2012-05-04 (1.7)
;;    remove hippie expand + add clearcase shortcut
;; 2012-05-03 (1.6)
;;    add shortcut to new functions for macros, and windows swap
;; 2012-03-30 (1.5)
;;    translate comments in English
;; 2012-03-29 (1.4)
;;    add new align shortcut
;; 2012-03-19 (1.3)
;;    add align shortcut and replace for rtrt
;; 2011-11-03 (1.2)
;;    add preprocessing function for each project
;; 2011-10-27 (1.1)
;;    comment the Magneti CAN Changelog insert
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;;; insert something in CAN changelog.txt (Magneti Marelli not used...)
;;(global-set-key "\C-cr"                 'mm-can-insert-del-signal)
;;(global-set-key "\C-cf"                 'mm-can-insert-add-signal)
;;(global-set-key "\C-ct"                 'mm-can-insert-del-message)
;;(global-set-key "\C-cg"                 'mm-can-insert-add-message)
;;(global-set-key "\C-cd"                 'mm-can-insert-separation)

;; select the whole word at point
(global-set-key         "\C-\M-z"           'select-word-under)
(global-set-key         "\C-\M-c"           'occur-word-at-point)

;; some setting are done after launch (only MS Windows)
(global-set-key         "\C-cl"             'mystart-up)

;; use align regexp for .ptu file (rtrt script)
(global-set-key         "\C-cpo"            'rtrt-align-init)
(global-set-key         "\C-cp;"            'rtrt-align-ev)
(global-set-key         "\C-cp["            'rtrt-align-declaration)
(global-set-key         "\C-cp="            'rtrt-align-set)

;; format the .ptu file (rtrt script)
(global-set-key         "\C-crv"            'rtrt-upcase-var-string)
(global-set-key         "\C-crs"            'rtrt-remove-whitespace-before-colon)

;;;; preprocess a C macro for NSF project
;;(global-set-key         "\C-cms"            'nsf-c-expand-macro)
;;;; preprocess a C macro for NBNF HL project
;;(global-set-key         "\C-cmh"            'nhl-c-expand-macro)
;;;; preprocess a C macro for NBNF LL project
;;(global-set-key         "\C-cmn"            'nll-c-expand-macro)
;;;; preprocess a C macro for ENSF project
;;(global-set-key         "\C-cme"            'ecar-c-expand-macro)
;;;; preprocess a C macro for XL1 project
;;(global-set-key         "\C-cmx"            'xl1-c-expand-macro)

;; macro (by Fabrice Niessen)
;; start/stop recording a keyboard macro (if you change it you also must change
;; it in functions.el)
(global-set-key         (kbd "<S-f8>")          'my-toggle-kbd-macro-recording-on)
;;
;; execute the most recent keyboard macro
(global-set-key         (kbd "<f8>")            'call-last-kbd-macro)
;;
;; assign a name to the last keyboard macro defined
(global-set-key         (kbd "<C-f8>")          'name-last-kbd-macro)

;;
;; swap 2 windows
(global-set-key         (kbd "C-c ~")           'my-swap-windows)
;; toggle the split (horizontal or vertical)
(global-set-key         (kbd "C-c |")           'my-toggle-window-split)

;;
;;; clearcase bind
;; checkout
(global-set-key         (kbd "C-c c c")         'clearcase-checkout-graphical)
;; diff
(global-set-key         (kbd "C-c c =")         'clearcase-diff-graphical)
;; history
(global-set-key         (kbd "C-c c l")         'clearcase-history-graphical)
;; uncheckout
(global-set-key         (kbd "C-c c u")         'clearcase-uncheckout-graphical)
;; version tree
(global-set-key         (kbd "C-c c L")         'clearcase-version-tree-graphical)
;; clearcase explorer
(global-set-key         (kbd "C-c c e")         'clearcase-explorer-graphical)
;; version properties
(global-set-key         (kbd "C-c c v")         'clearcase-version-properties-graphical)
;; element properties
(global-set-key         (kbd "C-c c p")         'clearcase-properties-graphical)
;; element properties
(global-set-key         (kbd "C-c c i")         'clearcase-checkin-graphical)
;; element properties
(global-set-key         (kbd "C-c c f")         'clearcase-find-checkout-graphical)

;; improve tab key for hide show mode
(define-key     hs-minor-mode-map       [tab]   'tab-hs-hide)

;;; shortcut-function.el ends here
