;;; 02-mode-049-fastnav.el --- configuration of mode

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
;;
;; [SUBHEADER.fast navigation like with zap-to-char but only to move]
;; [SUBDEFAULT.nil]


;;; Code:
(when (try-require 'autoload-fastnav "    ")
  ;; FIXME: put shortcut in hook
  ;;(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
  ;;(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
  (global-set-key "\M-s" 'fastnav-jump-to-char-forward)
  ;;(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
  ;;(global-set-key "\M-r" 'fastnav-replace-char-forward)
  ;;(global-set-key "\M-R" 'fastnav-replace-char-backward)
  ;;(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
  ;;(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
  ;;(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
  ;;(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
  ;;(global-set-key "\M-k" 'fastnav-delete-char-forward)
  ;;(global-set-key "\M-K" 'fastnav-delete-char-backward)
  ;;(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
  ;;(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
  ;;(global-set-key "\M-p" 'fastnav-sprint-forward)
  ;;(global-set-key "\M-P" 'fastnav-sprint-backward)
  )


(provide '02-mode-049-fastnav)

;;; 02-mode-049-fastnav.el ends here
