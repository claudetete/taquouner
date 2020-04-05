;;; 02-mode-035-org-mode.el --- configuration of org mode

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.to organize everything (also use on Android)]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package org
  :pin org

  :config
  ;; [VARCOMMENT.set org directory where every org file will goes]
  ;; [VARIABLE.tqnr-profile-org-directory (concat (file-name-as-directory tqnr-dotemacs-path) "org")]
  (setq org-directory tqnr-profile-org-directory)

  ;; [VARCOMMENT.default org file where all task/todo capture will goes]
  ;; [VARIABLE.tqnr-profile-org-default-notes-file (concat (file-name-as-directory tqnr-profile-org-directory) "my.org")]
  (setq org-default-notes-file tqnr-profile-org-default-notes-file)

  ;; [VARCOMMENT.agenda will look only in default org file]
  ;; [VARIABLE.tqnr-profile-org-agenda-files (concat (file-name-as-directory tqnr-profile-org-directory) "agenda.list")]
  ;; Which files do I want to be checked for entries when compiling my agenda
  ;; Can be customized with add current buffer "C-c [" or remove current buffer "C-c ]"
  (setq org-agenda-files tqnr-profile-org-agenda-files)

  ;; set timestamp when TODO is DONE
  (setq org-log-done t)
  ;; to not show schedule or deadline in agenda if task is done
  ;;(setq org-agenda-skip-scheduled-if-done t)
  ;;(setq org-agenda-skip-deadline-if-done t)
  ;; to use ido to do completion for org mode
  (setq org-completion-use-ido t)

  ;; [VARCOMMENT.first buffer to show up is default org file when opening emacs]
  ;; [VARIABLE.tqnr-section-mode-org-default-as-init-buffer t]
  (when tqnr-section-mode-org-default-as-init-buffer
    ;; use default org file to be the first initial buffer open by emacs
    (setq initial-buffer-choice org-default-notes-file)
    )

  :hook
  ;; Make windmove work in org-mode:
  (org-shiftup-final-hook    . windmove-up)
  (org-shiftleft-final-hook  . windmove-left)
  (org-shiftdown-final-hook  . windmove-down)
  (org-shiftright-final-hook . windmove-right)


  :bind (
          ;; to add from everywhere a note/todo into default org file `org-default-notes-file'
          ("C-c o c" . org-capture)
          ("C-c o o" . org-capture)
          ;; to open from everywhere an org file
          ("C-c o f" . org-iswitchb)

          :map org-mode-map
          ;; windmove work when S-left/right do nothing in org buffer
          ("<S-right>" . org-shiftright-dwim)
          ("<S-left>"  . org-shiftleft-dwim)

          ;; add new shortcut to open browse mode
          ("C-c <up>"   . org-goto)
          ("C-c <down>" . org-goto)
          )


(provide '02-mode-035-org-mode)

;;; 02-mode-035-org-mode.el ends here
