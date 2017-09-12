;;; 05-display-00-buffer.el --- a config file for window/buffer display setting

;; Copyright (c) 2006-2017 Claude Tete
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
;; Version: 2.0
;; Created: October 2006
;; Last-Updated: July 2017

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

;;; Change Log:
;; 2017-07-25 (2.0)
;;    update to new conf format
;; 2013-05-07 (1.9)
;;    remove Completion buffer to be in same window
;; 2013-02-05 (1.8)
;;    toggle compile ecb window when bookmark jump
;; 2012-08-01 (1.7)
;;    add visual line mode
;; 2012-07-09 (1.6)
;;    try new handler for completion buffer
;; 2012-06-14 (1.5)
;;    remove misunderstanding for same-window-buffer-names option
;; 2012-06-05 (1.4)
;;    move transparency to interface.el
;; 2012-05-04 (1.3)
;;    add three possibility of transparency
;; 2012-03-20 (1.2)
;;    translate comments in English
;; 2012-03-20 (1.1)
;;    add buffer name to list of compilation window
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


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
