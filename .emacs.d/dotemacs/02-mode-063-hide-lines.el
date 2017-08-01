;;; 02-mode-063-hide-lines.el --- configuration of hide lines mode

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
;; [SUBHEADER.hide lines using regexp (like narrow but with regex and not region)]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'autoload-hide-lines "    ")
  ;; can hide incrementaly
  (try-require 'autoload-hidesearch "    "))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; hide match
    (global-set-key     (kbd "C-c h h")         'hide-lines-matching)
    ;; hide not match
    (global-set-key     (kbd "C-c h n")         'hide-lines-not-matching)
    ;; show all
    (global-set-key     (kbd "C-c h s")         'hide-lines-show-all)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-063-hide-lines)

;;; 02-mode-063-hide-lines.el ends here
