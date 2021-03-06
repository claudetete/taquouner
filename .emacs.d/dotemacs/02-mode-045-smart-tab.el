;;; 02-mode-045-smart-tab.el --- configuration of smart tab mode -*- lexical-binding: t -*-

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
;; [SUBHEADER.expand or indent at the point with tab]
;; [SUBDEFAULT.nil]


;;; Code:
;; smart-tab mode has been patch to not change habits about tab key
;; Tab key once will indent like always
;; Tab key twice will try to expand the current 'expression'
(use-package smart-tab
  :config
  ;; use hippie expand see `07-completion.el'
  (setq smart-tab-using-hippie-expand t)
  ;; enable smart-tab mode everywhere
  ;;(global-smart-tab-mode t)
  ) ;; (use-package smart-tab


(provide '02-mode-045-smart-tab)

;;; 02-mode-045-smart-tab.el ends here
