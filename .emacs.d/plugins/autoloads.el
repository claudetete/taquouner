;;; autoloads.el --- a script file to generate autoload files

;; Copyright (c) 2013-2020 Claude Tete
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
;; Created: April 2013
;; Last-Updated: February 2020

;;; Commentary:
;;
;;  must be run before using emacs:
;;  emacs -Q -l autoloads.el -f autoloads

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
  ;; GNU/GLOBAL ggtags
  (update-file-autoloads "ggtags.el"                 t (concat autoloads-location "autoload-ggtags.el"))
  ;; AUTO HIGHLIGHT SYMBOL
  (update-file-autoloads "auto-highlight-symbol.el"  t (concat autoloads-location "autoload-auto-highlight-symbol.el"))
  ;; UNDO TREE
  (update-file-autoloads "undo-tree.el"              t (concat autoloads-location "autoload-undo-tree.el"))
  ;; DIFF COLOR
  (update-file-autoloads "diff-mode-.el"             t (concat autoloads-location "autoload-diff-mode-.el"))
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
  ;; AVY
  (update-file-autoloads "avy.el"                    t (concat autoloads-location "autoload-avy.el"))
  ;; RAINBOW MODE
  (update-file-autoloads "rainbow-mode.el"           t (concat autoloads-location "autoload-rainbow-mode.el"))
  ;; PLATINIUM SEARCH
  (update-file-autoloads "pt.el"                     t (concat autoloads-location "autoload-pt.el"))
  ;; FOLD-DWIM
  (update-file-autoloads "fold-dwim.el"              t (concat autoloads-location "autoload-fold-dwim.el"))
  ;; ACK-EMACS
  (update-file-autoloads "ack-emacs.el"              t (concat autoloads-location "autoload-ack-emacs.el"))
  ;; PS2PDF
  (update-file-autoloads "ps2pdf.el"                 t (concat autoloads-location "autoload-ps2pdf.el"))
  ;; SYNERGY
  (update-file-autoloads "synergy-web.el"            t (concat autoloads-location "autoload-synergy-web.el"))
  ;; HIDE-LINES
  (update-file-autoloads "hide-lines.el"             t (concat autoloads-location "autoload-hide-lines.el"))
  (update-file-autoloads "hidesearch.el"             t (concat autoloads-location "autoload-hidesearch.el"))
  ;; HELM PLATINIUM SEARCH
  (update-file-autoloads "helm-pt.el"                t (concat autoloads-location "autoload-helm-pt.el"))
  ;; HELM PLATINIUM SEARCH
  (update-file-autoloads "helm-ag.el"                t (concat autoloads-location "autoload-helm-ag.el"))
  ;; HELM COMPANY
  (update-file-autoloads "helm-company.el"           t (concat autoloads-location "autoload-helm-company.el"))
  ;; HELM GTAGS
  (update-file-autoloads "helm-gtags.el"             t (concat autoloads-location "autoload-helm-gtags.el"))
  ;; COMPANY
  (update-file-autoloads "company-mode/company.el"   t (concat autoloads-location "company-mode/autoload-company.el"))
  ;; MARKDOWN
  (update-file-autoloads "markdown-mode.el"          t (concat autoloads-location "autoload-markdown-mode.el"))
  ;; EASY-KILL
  (update-file-autoloads "easy-kill.el"              t (concat autoloads-location "autoload-easy-kill.el"))
  ;; PROJECTILE
  (update-file-autoloads "projectile-master/projectile.el" t
    (concat autoloads-location "projectile-master/autoload-projectile.el"))
  ;; HELM PROJECTILE
  (update-file-autoloads "helm-projectile.el"        t (concat autoloads-location "autoload-helm-projectile.el"))
  ;; UNDO FU
  (update-file-autoloads "undo-fu.el"                t (concat autoloads-location "autoload-undo-fu.el"))
  ;; UNDO FU SESSION
  (update-file-autoloads "undo-fu-session.el"        t (concat autoloads-location "autoload-undo-fu-session.el"))

  ;; quit emacs
  (kill-emacs)
  )

;;; autoloads.el ends here
