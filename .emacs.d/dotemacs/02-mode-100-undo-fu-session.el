;;; 02-mode-100-undo-fu-session.el --- configuration of undo fu session mode

;; Copyright (c) 2020 Claude Tete
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
;; Created: February 2020
;; Last-Updated: February 2020

;;; Commentary:
;;
;; [SUBHEADER.Save & recover undo steps between Emacs sessions]
;; [SUBDEFAULT.nil]


;;; Code:
(when (try-require 'autoload-undo-fu-session "    ")
  (global-undo-fu-session-mode)

  ;; [VARCOMMENT.set undo fu session directory where every undo will be stored]
  ;; [VARIABLE.tqnr-profile-undo-fu-session-directory (concat (file-name-as-directory tqnr-dotemacs-path) "undo")]
  (setq undo-fu-session-directory tqnr-profile-undo-fu-session-directory)

  ;; [VARCOMMENT.set org directory where every org file will goes]
  ;; [VARIABLE.tqnr-profile-undo-fu-session-file-limit 1024]
  (setq undo-fu-session-file-limit tqnr-profile-undo-fu-session-file-limit)
  )


(provide '02-mode-099-undo-fu)

;;; 02-mode-099-undo-fu.el ends here
