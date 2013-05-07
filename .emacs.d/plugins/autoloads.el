;;; autoloads.el --- a script file to generate autoload files

;; Copyright (c) 2013 Claude Tete
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

;; Keywords: config, autoload, generated, load
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 0.1
;; Created: April 2013
;; Last-Updated: April 2013

;;; Commentary:
;;
;;  must be run before using emacs:
;;  emacs -Q -l autoloads.el -f autoloads

;;; Change Log:
;; 2013-04-12 (0.1)
;;    creation from scratch


;;; Code:

;; no more ~file and #file#
(setq make-backup-files nil)
(auto-save-mode nil)

;; get the current directory path
(defvar autoloads-location
  (let ((dir (file-name-directory
               (or load-file-name (buffer-file-name)))))
    dir)
  "Path of plugin directory.")

(defun autoloads ()
  "Generate all autoload files for this configuration."
  ;; BROWSE KILL RING
  (update-file-autoloads "browse-kill-ring.el"       t (concat autoloads-location "autoload-browse-kill-ring.el"))
  ;; DIRED+
  (update-file-autoloads "dired+.el"                 t (concat autoloads-location "autoload-dired+.el"))
  ;; GNU/GLOBAL
  (update-file-autoloads "gtags.el"                  t (concat autoloads-location "autoload-gtags.el"))
  ;; AUTO HIGHLIGHT SYMBOL
  (update-file-autoloads "auto-highlight-symbol.el"  t (concat autoloads-location "autoload-auto-highlight-symbol.el"))
  ;; UNDO TREE
  (update-file-autoloads "undo-tree.el"              t (concat autoloads-location "autoload-undo-tree.el"))
  ;; DIFF COLOR
  (update-file-autoloads "diff-mode-.el"             t (concat autoloads-location "autoload-dired-mode-.el"))
  ;; DIRED SORT
  (update-file-autoloads "dired-sort-menu.el"        t (concat autoloads-location "autoload-dired-sort-menu.el"))
  ;; ISEARCH+
  (update-file-autoloads "isearch+.el"               t (concat autoloads-location "autoload-isearch+.el"))
  ;; PSVN
  (update-file-autoloads "psvn.el"                   t (concat autoloads-location "autoload-psvn.el"))
  ;; NYAN
  (update-file-autoloads "nyan-mode.el"              t (concat autoloads-location "autoload-nyan-mode.el"))
  ;; SML
  (update-file-autoloads "sml-modeline.el"           t (concat autoloads-location "autoload-sml-modeline.el"))
  ;; RAINBOW DELIMITERS
  (update-file-autoloads "rainbow-delimiters.el"     t (concat autoloads-location "autoload-rainbow-delimiters.el"))
  ;; DIRED DETAILS +
  (update-file-autoloads "dired-details+.el"         t (concat autoloads-location "autoload-dired-details+.el"))
  ;; SMART TAB
  (update-file-autoloads "smart-tab.el"              t (concat autoloads-location "autoload-smart-tab.el"))
  ;; DIRED LETTER ISEARCH
  (update-file-autoloads "dired-lis.el"              t (concat autoloads-location "autoload-dired-lis.el"))
  ;; FASTNAV
  (update-file-autoloads "fastnav.el"                t (concat autoloads-location "autoload-fastnav.el"))
  ;; ACE JUMP
  (update-file-autoloads "ace-jump-mode.el"          t (concat autoloads-location "autoload-ace-jump-mode.el"))

  ;; quit emacs
  (kill-emacs)
  )

;;; autoloads.el ends here
