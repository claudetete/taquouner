;;; 12-project-platform.el --- a config file for project platform settings -*- lexical-binding: t -*-

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
;; Created: March 2023
;; Last-Updated: March 2023

;;; Commentary:
;;
;; [[HEADER.Configure emacs for a specific platform for a project (one by instance)
;; ]]
;; [DEFAULT.t]


;;; Code:
;; tqnr-profile-project and tqnr-profile-project-platform should be set on command line or in ~/.emacs
(when (and (boundp 'tqnr-profile-project) (boundp 'tqnr-profile-project-platform))
  ;; add to load path the dotemacs directory
  (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "project"))

  ;; build config file path and filename
  (let ((config-file-path (concat (file-name-as-directory tqnr-dotemacs-path) "project/" tqnr-profile-project "-" tqnr-profile-project-platform ".el"))
        (config-file (intern (concat tqnr-profile-project "-" tqnr-profile-project-platform)))
        (readme-file (concat (file-name-as-directory (getenv "VIEWPATH")) "readme.md")))
    ;; load only if the file exists
    (if (not (and (file-exists-p config-file-path) (try-require config-file "  ")))
        (display-warning 'tqnr-profile-project (concat (concat tqnr-profile-project "-" tqnr-profile-project-platform) " project/platform configuration do not exist") :warning)
      ;; when load is successful, display at first the readme of the project
      (when (file-exists-p readme-file)
        (setq initial-buffer-choice readme-file)))))


(provide '12-project-platform)

;;; 12-project-platform.el ends here
