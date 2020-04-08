;;; 02-mode-091-org-jira.el --- configuration of org jira -*- lexical-binding: t -*-

;; Copyright (c) 2018-2020 Claude Tete
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
;; Created: January 2018
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Sync JIRA issues with org-mode issues]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package org-jira
  :config
  (setq jiralib-url "http://jiratav2.fr.thav.thales:8080"))


(provide '02-mode-091-org-jira)

;;; 02-mode-091-org-jira.el ends here
