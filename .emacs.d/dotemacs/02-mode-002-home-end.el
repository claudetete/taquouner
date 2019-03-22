;;; 02-mode-002-home-end.el --- configuration of home/end keys mode

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
;; [SUBHEADER.add some useful function to home and end keys]
;; [SUBDEFAULT.t]


;;; Code:
;; to add features to home/end key (two push will get you at the end/start
;; of display) (three push will get you at the end/start of buffer)
(try-require 'pc-keys "    ")


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; bind home with new features
    (global-set-key     (kbd "<home>")  'pc-keys-home)

    ;; bind home with new features
    (global-set-key     (kbd "<end>")   'pc-keys-end)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-002-home-end)

;;; 02-mode-002-home-end.el ends here
