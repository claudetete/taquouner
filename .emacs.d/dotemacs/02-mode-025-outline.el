;;; 02-mode-025-outline.el --- configuration of outline mode

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
;; [SUBHEADER.to manually hide some block in code source]
;; [SUBDEFAULT.nil]


;;; Code:
(outline-minor-mode 1)

;; [VARCOMMENT.HIDE ALL AT START: hide all when opening file]
;; [VARIABLE.tqnr-section-mode-outline-hide-all-at-start nil]
(when tqnr-section-mode-outline-hide-all-at-start
  (hide-sublevels 1))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (when (not tqnr-section-mode-hydra-outline)
      ;; bind toggle hide/show block
      (global-set-key     (kbd "C-c h")   'outline-toggle-children)
      ) ;; (when (not tqnr-section-mode-hydra-outline)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-025-outline)

;;; 02-mode-025-outline.el ends here
