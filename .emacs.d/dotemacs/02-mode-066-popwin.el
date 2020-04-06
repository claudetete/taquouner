;;; 02-mode-066-popwin.el --- configuration of popwin mode

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
;; Version: 0.6
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.A pop-up manager for annoying buffer (have like ECB compilation buffer)]
;; [SUBDEFAULT.t]


;;; Code:
(use-package popwin
  :init
  (popwin-mode 1)

  :config
  (push '(compilation-mode :noselect t :stick t) popwin:special-display-config)
  (push '("*Shell Command Output*" :stick t) popwin:special-display-config)
  (push '(dired-mode :stick t) popwin:special-display-config)
  (push '("*Messages*" :stick t) popwin:special-display-config)
  (push '("*compilation*" :stick t) popwin:special-display-config)
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
  (push '("\\s-*\\*magit.*\\*\\s-*" :regexp t :height 0.3 :stick t) popwin:special-display-config)
  (push '("\\s-*magit-process:.*" :regexp t :height 0.3 :stick t) popwin:special-display-config)
  (push '("\\s-*magit:.*" :regexp t :height 0.3 :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-build-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-check-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-pretty-print-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-build-all-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-clean-all-buffer-name :stick t) popwin:special-display-config)
  (push '(tqnr-ada-gps-build-native-buffer-name :stick t) popwin:special-display-config)
  (push '("\\s-*\\*helpful.*\\*\\s-*" :regexp t :stick t) popwin:special-display-config)
  (push '("\\s-*\\*i?pdb.*\\*\\s-*" :regexp t :stick t) popwin:special-display-config)
  ) ;; (use-package popwin


(provide '02-mode-066-popwin)

;;; 02-mode-066-popwin.el ends here
