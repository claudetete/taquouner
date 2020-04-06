;;; 05-display-00-buffer.el --- a config file for window/buffer display setting

;; Copyright (c) 2006-2019 Claude Tete
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
;; Version: 2.0
;; Created: October 2006
;; Last-Updated: March 2019

;;; Commentary:
;; section comment
;; [HEADER.modification about display in buffers, font, color...]
;; [DEFAULT.t]
;;
;; subsection comment
;; [[SUBHEADER.buffers with *buffername* should be displayed in the same window
;; first column in window will display buffer limit, next page will leave 5 shared line
;; ]]
;; [SUBDEFAULT.t]


;;; Code:
;; Define buffers that should appear in the same window than the window caller.
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "*Colors*")

;; display an arrows or a corner at left to show beginning and ending of a
;; file
(setq-default indicate-buffer-boundaries (quote left))

;; after a PageUp or Down, it will display 5 shared lines
(custom-set-variables
  '(next-screen-context-lines 5))

;; hide compile window when jump to a bookmark
(when tqnr-section-mode-cedet-ecb
  (add-hook 'bookmark-after-jump-hook 'ecb-toggle-compile))

;;
;; [[VARCOMMENT.VISUAL LINE: word wrap, truncate line without cut word
;; END and HOME will go to the end/start of screen line not logical line
;; ]]
;; [VARIABLE.tqnr-section-display-windows-buffers-visual-line nil]
(when tqnr-section-display-windows-buffers-visual-line (message "      Visual Line...")
  ;; enable visual line
  (global-visual-line-mode t)
  ;; show fringe indicators for wrapped line
  (setq visual-line-fringe-indicators t)
  (message "      Visual Line... Done"))


(provide '05-display-00-buffer)

;;; 05-display-00-buffer.el ends here
