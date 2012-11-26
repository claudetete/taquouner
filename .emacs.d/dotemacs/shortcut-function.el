;;; shortcut-function.el --- a config file for function shortcut

;; Copyright (c) 2006-2012 Claude Tete
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
;; Version: 2.7
;; Created: October 2006
;; Last-Updated: November 2012

;;; Commentary:
;;
;; load by `dotemacs/shortcut.el'
;; REQUIREMENT: var     `section-shortcut-function'
;;              var     `section-shortcut'

;;; Change Log:
;; 2012-11-26 (2.7)
;;    logical shortcut for web search
;; 2012-10-31 (2.6)
;;    add new functions to navigate through rtrt files
;; 2012-08-01 (2.5)
;;    clean up + move shortcuts + shortcut for macro + some conditions + case
;;    shortcut
;; 2012-06-26 (2.4)
;;    add shortcut for google search
;; 2012-06-21 (2.3)
;;    add shortcut for translate, synonym, conjugate and wikipedia function
;; 2012-06-14 (2.2)
;;    clean up
;; 2012-06-12 (2.1)
;;    remove shortcut with TAB in hide/show mode
;; 2012-06-08 (2.0)
;;    fix bug about word selection, isearch word + add scroll without moving
;;    cursor
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
;;
;; select the whole word at point
(global-set-key         "\C-\M-z"               'select-word-under)

;;
;;; RTRT SCRIPT
(when section-mode-rtrt-script
  ;; use align regexp for .ptu file (rtrt script)
  (global-set-key       "\C-cpo"                'rtrt-align-init)
  (global-set-key       "\C-cp;"                'rtrt-align-ev)
  (global-set-key       "\C-cp["                'rtrt-align-declaration)
  (global-set-key       "\C-cp="                'rtrt-align-set)
  ;;
  ;; format the .ptu file (rtrt script)
  (global-set-key       "\C-crv"                'rtrt-upcase-var-string)
  (global-set-key       "\C-crs"                'rtrt-remove-whitespace-before-colon)
  ;;
  ;; move
  (add-hook 'rtrt-script-mode-hook
    '(lambda ()
       (local-set-key   (kbd "<M-left>")        'rtrt-up-test-header)
       (local-set-key   (kbd "<M-right>")       'rtrt-down-test-header)
       (local-set-key   (kbd "<M-up>")          'rtrt-up-heading)
       (local-set-key   (kbd "<M-down>")        'rtrt-down-heading)))
  (add-hook 'nxml-mode-hook
    '(lambda ()
       (when (string-match "\\.rtp$" (buffer-name))
         (local-set-key   (kbd "<M-left>")        'rtrt-rtp-up-heading)
         (local-set-key   (kbd "<M-right>")       'rtrt-rtp-down-heading))))
  ) ; (when section-mode-rtrt-script

;;
;;; PREPROC MM
(when section-function-mm
  ;; preprocess a C macro for NSF project
  (global-set-key       "\C-cms"            'nsf-c-expand-macro)
  ;; preprocess a C macro for NBNF HL project
  (global-set-key       "\C-cmh"            'nhl-c-expand-macro)
  ;; preprocess a C macro for NBNF LL project
  (global-set-key       "\C-cmn"            'nll-c-expand-macro)
  ;; preprocess a C macro for ENSF project
  (global-set-key       "\C-cme"            'ecar-c-expand-macro)
  ;; preprocess a C macro for XL1 project
  (global-set-key       "\C-cmx"            'xl1-c-expand-macro)
  ) ; (when section-function-mm

;;
;;; MACRO
;; macro (by Fabrice Niessen)
;; start/stop recording a keyboard macro (if you change it you also must change
;; it in functions.el)
(global-set-key         (kbd "<S-f8>")          'my-toggle-kbd-macro-recording-on)
;; execute the most recent keyboard macro or on each line if a region is
;; selected
(global-set-key         (kbd "<f8>")            'call-last-kd-macro-region)
;; assign a name to the last keyboard macro defined
(global-set-key         (kbd "<C-f8>")          'name-last-kbd-macro)
;; edit the last keyboard macro defined
(global-set-key         (kbd "<M-f8>")          'edit-last-kbd-macro)
;; select previous macro
(global-set-key         (kbd "<H-f8>")          'kmacro-cycle-ring-previous)
;; select next macro
(global-set-key         (kbd "<H-S-f8>")        'kmacro-cycle-ring-next)

;;
;;; WINDOWS
;; swap 2 windows
(global-set-key         (kbd "C-c ~")           'my-swap-windows)
;; toggle the split (horizontal or vertical)
(global-set-key         (kbd "C-c |")           'my-toggle-window-split)

;;
;;; CLEARCASE
(when (or section-mode-clearcase section-mode-vc-clearcase)
  ;; checkout
  (global-set-key       (kbd "C-c c c")         'clearcase-checkout-graphical)
  ;; diff
  (global-set-key       (kbd "C-c c =")         'clearcase-diff-graphical)
  ;; history
  (global-set-key       (kbd "C-c c l")         'clearcase-history-graphical)
  ;; uncheckout
  (global-set-key       (kbd "C-c c u")         'clearcase-uncheckout-graphical)
  ;; version tree
  (global-set-key       (kbd "C-c c L")         'clearcase-version-tree-graphical)
  ;; clearcase explorer
  (global-set-key       (kbd "C-c c e")         'clearcase-explorer-graphical)
  ;; version properties
  (global-set-key       (kbd "C-c c v")         'clearcase-version-properties-graphical)
  ;; element properties
  (global-set-key       (kbd "C-c c p")         'clearcase-properties-graphical)
  ;; element properties
  (global-set-key       (kbd "C-c c i")         'clearcase-checkin-graphical)
  ;; element properties
  (global-set-key       (kbd "C-c c f")         'clearcase-find-checkout-graphical)
  ) ; (when (or section-mode-clearcase section-mode-vc-clearcase)

;;
;;; SCROLL
;; scroll while keeping cursor position
(global-set-key         (kbd "<H-down>")        'scroll-down-keep-cursor)
(global-set-key         (kbd "<H-up>")          'scroll-up-keep-cursor)

;;
;;; WEB SEARCH
;; translate word at point or region
(global-set-key         (kbd "<f12>")           'translate-enfr)
(global-set-key         (kbd "<M-f12>")         'translate-fren)
;;
;; synonym
(global-set-key         (kbd "<S-f12>")         'synonym-fr)
;;
;; grammatical conjugation
(global-set-key         (kbd "<C-f12>")         'conjugation-fr)
;;
;; wikipedia
(global-set-key         (kbd "<f11>")           'wikipedia-en)
(global-set-key         (kbd "<M-f11>")         'wikipedia-fr)
;;
;; google
(global-set-key         (kbd "<C-M-f11>")       'google-fr)
(global-set-key         (kbd "<C-f11>")         'google-en)

;;
;;; CASE
;; upper case word or region
(global-set-key         (kbd "M-u")             'case-up)
;; down case word or region
(global-set-key         (kbd "M-l")             'case-down)
;; capitalize case word or region
(global-set-key         (kbd "M-c")             'case-capitalize)

;;
;;; SEARCH
;; navigate to start/end of balance expressions
(global-set-key (kbd "M-[") 'find-matching-keyword)
(global-set-key (kbd "M-]") 'find-matching-keyword)


(provide 'shortcut-function)

;;; shortcut-function.el ends here
