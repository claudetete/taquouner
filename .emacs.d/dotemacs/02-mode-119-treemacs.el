;;; 02-mode-119-treemacs.el --- configuration of treemacs -*- lexical-binding: t -*-

;; Copyright (c) 2023 Claude Tete
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
;; Version: 0.1
;; Created: May 2023
;; Last-Updated: May 2023

;;; Commentary:
;;
;; [SUBHEADER.treemacs: display file tree on one side]
;; [SUBDEFAULT.nil]


;;; Code:
(defun my/treemacs-find-file ()
  (interactive)
  (treemacs-find-file)
  (treemacs-select-window))



(use-package treemacs
  :config
  (setq treemacs-follow-mode 1)

  (use-package treemacs-all-the-icons)

  (use-package treemacs-project-follow-mode
    :ensure nil
    :config
    (treemacs-project-follow-mode t))

  (defun treemacs-ignore-files (file _)
    (or (s-ends-with? ".elc" file)
        (s-ends-with? ".o" file)
        (s-ends-with? ".a" file)
        (string= file ".svn")))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-files)

  (defun treemacs-expand-or-goto- ()
    (interactive "P")
    (treemacs-do-for-button-state
     :on-root-node-open   (treemacs--collapse-root-node btn arg)
     :on-dir-node-open    (treemacs--collapse-dir-node btn arg)
     :on-dir-node-closed  (treemacs-goto-parent-node)
     :on-file-node-open   (treemacs--collapse-file-node btn arg)
     :on-file-node-closed (treemacs-goto-parent-node)
     :on-tag-node-open    (treemacs--collapse-tag-node btn arg)
     :on-tag-node-closed  (treemacs-goto-parent-node)
     :on-tag-node-leaf    (treemacs-goto-parent-node)
     :fallback            (treemacs-TAB-action)))

  :custom
  ;; configure default width
  (treemacs-width 40)
  ;; enable resizing
  (treemacs-width-is-initially-locked nil)
  ;; close treemacs when the window is selected and call `treemacs-select-window'
  (treemacs-select-when-already-in-treemacs 'close)
  (treemacs-fringe-indicator-mode t)
  (treemacs-is-never-other-window t)
  (treemacs-silent-refresh    t)

  :bind
  ("M-1" . treemacs-select-window)
  (:map treemacs-mode-map
        ;; collapse when opened, goto parent when closed
        ("<left>"  . treemacs-COLLAPSE-action)
        ("<right>" . treemacs-goto-parent-node)
        )
  ;; ("C-<f9>" . my/treemacs-find-file)
  )

(when tqnr-section-mode-projectile
  (use-package treemacs-projectile))

(provide '02-mode-119-treemacs)

;;; 02-mode-119-treemacs.el ends here
