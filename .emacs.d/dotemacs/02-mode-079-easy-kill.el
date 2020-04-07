;;; 02-mode-079-easy-kill.el --- configuration of easy kill mode -*- lexical-binding: t -*-

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
;; [SUBHEADER.mode to easy copy/kill/cut text/line/word/expression/function...]
;; [SUBDEFAULT.t]


;;; Code:
(use-package easy-kill
  :pin melpa

  :config
  ;; remap shortcut of M-w to easy-kill function
  ;; M-w: alone saves in the order of active region, url, email and finally current line
  ;; M-w w: save word at point
  ;; M-w s: save sexp at point
  ;; M-w l: save list at point (enclosing sexp)
  ;; M-w d: save defun at point
  ;; M-w D: save current defun name
  ;; M-w f: save file at point
  ;; M-w b: save buffer-file-name or default-directory. - changes the kill to
  ;;        the directory name, + to full name and 0 to basename.
  (global-set-key [remap kill-ring-save] 'easy-kill)
  ) ;; (use-package easy-kill


(provide '02-mode-079-easy-kill)

;;; 02-mode-079-easy-kill.el ends here
