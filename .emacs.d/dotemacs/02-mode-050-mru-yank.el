;;; 02-mode-050-mru-yank.el --- configuration of MRU yank mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.(Most Recently Used) in kill-ring]
;; [SUBDEFAULT.t]


;;; Code:
(use-package MRU-yank
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/MRU-yank.el"))

  :config
  (setq MRU-yank-mode t))


(provide '02-mode-050-mru-yank)

;;; 02-mode-050-mru-yank.el ends here
