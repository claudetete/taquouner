;;; 02-mode-081-all-the-icons.el --- configuration of arduino mode

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
;; Created: August 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [[SUBHEADER.mode to have nice icons (from special fonts)
;; install font on your system from `fonts' folder or use
;; M-x all-the-icons-install-fonts
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/all-the-icons"))
(when (try-require 'all-the-icons "    ")
  (setq inhibit-compacting-font-caches t)
  )


(provide '02-mode-081-all-the-icons)

;;; 02-mode-081-all-the-icons.el ends here
