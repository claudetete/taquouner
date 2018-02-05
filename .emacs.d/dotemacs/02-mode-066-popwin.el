;;; 02-mode-066-popwin.el --- configuration of popwin mode

;; Copyright (c) 2017-2018 Claude Tete
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
;; Version: 0.5
;; Created: July 2017
;; Last-Updated: January 2018

;;; Commentary:
;;
;; [SUBHEADER.A pop-up manager for annoying buffer (have like ECB compilation buffer)]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2018-01-31 (0.5)
;;    add magit and ada command function
;; 2017-09-19 (0.4)
;;    move F2 shortcut to function buffer/window
;; 2017-09-14 (0.3)
;;    use patch of popwin to show it only by splitting current window
;; 2017-09-11 (0.2)
;;    remove helm window (not very friendly with popwin, use shackle instead)
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'popwin "    ")
  (popwin-mode 1)

  (push '(compilation-mode :noselect t :stick t) popwin:special-display-config)
  (push '("*Shell Command Output*" :stick t) popwin:special-display-config)
  (push '(dired-mode :stick t) popwin:special-display-config)
  (push '("*Messages*" :stick t) popwin:special-display-config)
  (push '("*Calculator*" :stick t) popwin:special-display-config)
  (push "*vc*" popwin:special-display-config)
  (push "*vc-diff*" popwin:special-display-config)
  (push '("*Apropos*" :stick t) popwin:special-display-config)
  (push '("*Occur*" :stick t :inside t) popwin:special-display-config)
  (push "*shell*" popwin:special-display-config)
  (push '("*Help*" :stick t) popwin:special-display-config)
  (push '("*Backtrace*" :stick t) popwin:special-display-config)
  (push "*Compile-log*" popwin:special-display-config)
  (push '("*[Ss]ynergy*" :regexp r :stick t) popwin:special-display-config)
  (push '("\\s-*\\*[Ss]ynergy.*\\*\\s-*" :regexp r :stick t) popwin:special-display-config)
  (push '("\\s-*\\*[cC]ompletions*\\*\\s-*" :regexp t :inside t) popwin:special-display-config)
  (push '("\\*[cC]ompilation.*\\*" :regexp t :stick t) popwin:special-display-config)
  (push '("\\*i?grep.*\\*" :regexp t :stick t) popwin:special-display-config)
  (push "*JDEE Compile Server*" popwin:special-display-config)
  (push "*ccm*" popwin:special-display-config)
  (push '("\\*GTAGS SELECT\\*.*" :regexp t :stick t) popwin:special-display-config)
  (push "*Bookmark List*" popwin:special-display-config)
  (push "*Semantic Context Analyze*" popwin:special-display-config)
  (push "*Macroexpansion*" popwin:special-display-config)
  (push '("\\*Symref .*" :regexp t :stick t) popwin:special-display-config)
  (push "*ECB Analyse*" popwin:special-display-config)
  (push "*Kill Ring*" popwin:special-display-config)
  (push "*clearcase-config-spec*" popwin:special-display-config)
  (push "*ELP Profiling Results*" popwin:special-display-config)
  (push '("$\\s-*\\*[mM]agit.*\\*\\s-*" :regexp t :stick t) popwin:special-display-config)
  (push '("*pt-search*" :stick t) popwin:special-display-config)
  (push '("$\\s-*\\*[Aa]ck\\*\\s-*" :regexp t :stick t) popwin:special-display-config)
  (push '("$\\s-*\\*[Gg]tags.*\\*" :regexp t :stick t) popwin:special-display-config)
  ;; display undo-tree-visualize in a popwindow at right with only 10% of width
  (push '("\\s-*\\*undo-tree.*" :regexp t :width 0.1 :position right) popwin:special-display-config)
  (push '("*Python Doc*" :stick t) popwin:special-display-config)
  (push '("*haskell*" :stick t) popwin:special-display-config)
  (push '("*debug:haskell*" :stick t) popwin:special-display-config)
  (push '("*magit async*" :height 3) popwin:special-display-config)
  (push '(tqnr-ada-gps-build-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-check-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-pretty-print-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-build-all-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-clean-all-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-build-native-buffer-name :stick t) popwin:special-display-config)
  )


(provide '02-mode-066-popwin)

;;; 02-mode-066-popwin.el ends here
