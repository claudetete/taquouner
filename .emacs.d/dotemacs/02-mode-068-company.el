;;; 02-mode-068-company.el --- configuration of company mode -*- lexical-binding: t -*-

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Completion mode using external back-ends to have symbol]
;; [SUBDEFAULT.nil]
;;
;; TODO: do not use elpa path


;;; Code:
(use-package company
  :hook
  (after-init . global-company-mode)

  :config
  (setq company-backends (delete 'company-semantic company-backends))
  (when tqnr-section-mode-helm
    (use-package helm-company
      :bind (
              ;; use company even when helm is not used
              ("C-/" . company-complete)
              :map company-mode-map
              ;; use helm with company mode
              ("C-/" . helm-company)
              :map company-active-map
              ("C-/" . helm-company))
      ))

  ;; when irony mode is used
  (when tqnr-section-mode-irony
    (add-to-list 'company-backends 'company-irony))
  ) ;; (use-package company


(provide '02-mode-068-company)

;;; 02-mode-068-company.el ends here
