;;; 02-mode-097-move-text.el --- configuration of mvoe-text mode

;; Copyright (c) 2019 Claude Tete
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
;; Created: May 2019
;; Last-Updated: May 2019

;;; Commentary:
;;
;; [SUBHEADER.realgud mode]
;; [SUBDEFAULT.nil]


;;; Code:
(when (try-require 'move-text "    ")

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      (global-set-key (kbd "<C-M-S-up>")        #'move-text-up)
      (global-set-key (kbd "<C-M-S-down>")      #'move-text-down)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )

(provide '02-mode-097-move-text)

;;; 02-mode-097-move-text.el ends here
