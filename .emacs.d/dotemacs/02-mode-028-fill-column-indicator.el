;;; 02-mode-028-fill-column-indicator.el --- configuration of fill column indicator mode

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
;; [[SUBHEADER.show a line at fill-column (set at 80 in
;; dotemacs/11-misc.el be careful it enable truncate line
;; ]]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'fill-column-indicator)
  ;; width of line
  (setq fci-rule-width 1)
  ;; color of line
  (setq fci-rule-color "grey15")
  ;; only in C mode
  (add-hook 'c-mode-hook 'fci-mode)
  (add-hook 'c++-mode-hook 'fci-mode)
  ;;;; to show for all files
  ;;(add-hook 'after-change-major-mode-hook 'fci-mode)
  )


(provide '02-mode-028-fill-column-indicator)

;;; 02-mode-028-fill-column-indicator.el ends here
