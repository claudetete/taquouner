;;; 02-mode-002-home-end.el --- configuration of home/end keys mode

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
;; [SUBHEADER.add some useful function to home and end keys]
;; [SUBDEFAULT.t]


;;; Code:
;; to add features to home/end key (two push will get you at the end/start
;; of display) (three push will get you at the end/start of buffer)
(use-package pc-keys
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/pc-keys.el"))

  :bind
    ;; bind home with new features
    ("<home>" . pc-keys-home)

    ;; bind home with new features
    ("<end>" . pc-keys-end)
  ) ;; (use-package pc-keys


(provide '02-mode-002-home-end)

;;; 02-mode-002-home-end.el ends here
