;;; 02-mode-102-lsp-mode.el --- configuration of lsp-mode -*- lexical-binding: t -*-

;; Copyright (c) 2020 Claude Tete
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
;; Version: 0.0
;; Created: April 2020
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Lsp mode]
;; [SUBDEFAULT.nil]


;;; Code:
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
          (ada-mode . lsp))

  :commands lsp

  :config
  (when tqnr-section-mode-ada
    (defgroup project-build nil
      "LSP options for Project"
      :group 'ada-mode)

    (defcustom project-build-type "Debug"
      "Controls the type of build of a project.
   Default is Debug, other choices are Release and Coverage."
      :type '(choice
               (const "Debug")
               (const "Coverage")
               (const "Release"))
      :group 'project-build)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp
  :config
  (setq company-minimum-prefix-length 1
    company-idle-delay 0.0) ;; default is 0.2)
  )
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


(provide '02-mode-102-lsp-mode)

;;; 02-mode-102-lsp-mode.el ends here
