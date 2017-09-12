;;; 02-mode-069-expand-region.el --- configuration of expand region mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [SUBHEADER.Increase selected region by semantic units]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-09-11 (0.2)
;;    add shortcut about contract region/selection
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/expand-region"))
(when (try-require 'expand-region "    ")
  ;; see shortcut-global.el for settings
  )


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; M-s then s, s, etc to expand selection region
    (global-set-key     (kbd "M-s")     'er/expand-region)
    ;; M-S to contract expanded selection region
    (global-set-key     (kbd "M-S")     'er/contract-region)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-069-expand-region)

;;; 02-mode-069-expand-region.el ends here
