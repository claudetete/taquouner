;;; 00-environment-01-shortcut.el --- environment settings about shortcuts

;; Copyright (c) 2017-2019 Claude Tete
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
;; Last-Updated: March 2019

;;; Commentary:
;; [SUBHEADER.Environment shortcut to declare hook about shortcut]
;; [SUBDEFAULT.t]


;;; Code:

;; declare hook about shortcut to be done after init
(defvar tqnr-after-init-shortcut-hook nil
  "Normal hook run after configurationthe of Emacs")

;; Use following example to declare binding/shorcut in other config files:
;; ;; shortcuts are put in a hook to be loaded after everything else in init process
;; (add-hook 'tqnr-after-init-shortcut-hook
;;   (lambda ()
;;     (global-set-key     (kbd "<C-f8>")          'a-function-to-bind)
;;     ) ;; (lambda ()
;;   ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '00-environment-01-shortcut)

;;; 00-environment-01-shortcut.el ends here
