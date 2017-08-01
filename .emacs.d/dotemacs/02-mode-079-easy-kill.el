;;; 02-mode-079-easy-kill.el --- configuration of easy kill mode

;; Copyright (c) 2017 Claude Tete
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
;; Version: 0.1
;; Created: July 2017
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.mode to easy copy/kill/cut text/line/word/expression/function...]

;;; Change Log:
;; 2017-07-25 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'autoload-easy-kill "    ")
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
  )


(provide '02-mode-079-easy-kill)

;;; 02-mode-079-easy-kill.el ends here
