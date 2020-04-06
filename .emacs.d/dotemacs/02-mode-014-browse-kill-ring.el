;;; 02-mode-014-browse-kill-ring.el --- configuration of browse kill ring mode

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
;; [SUBHEADER.mode to browse the kill ring memory yank only on the first left top window...]
;; [SUBDEFAULT.t]


;;; Code:
(use-package browse-kill-ring
  :bind (:map browse-kill-ring-mode-map
          ;; move next and previous with arrow
          ("<up>" . browse-kill-ring-previous)
          ("<down>" . browse-kill-ring-forward)
          ;; quit not only with q but also with C-g or F2 with ecb
          ("C-g" . browse-kill-ring-quit)
          ("<escape>" . browse-kill-ring-quit)
          ("<return>" . browse-kill-ring-insert-move-and-quit))

  :config
  ;; all settings from Fabrice Niessen
  ;; string separating entries in the `separated' style
  (setq browse-kill-ring-separator
    "\n--separator------------------------------")
  ;; temporarily highlight the inserted `kill-ring' entry
  (setq browse-kill-ring-highlight-inserted-item t)
  (when (not tqnr-section-mode-helm-kill-ring)
    ;; use `M-y' to invoke `browse-kill-ring'
    (browse-kill-ring-default-keybindings))
  ;; do not show duplicate in list
  (setq browse-kill-ring-display-duplicates nil)
  ;; do not add duplicate in kill ring
  (setq browse-kill-ring-no-duplicates t)
  )

(when tqnr-section-mode-cedet-ecb
  (use-package browse-kill-ring
    :bind (:map browse-kill-ring-mode-map
            ("<f2>" . browse-kill-ring-quit))))


(provide '02-mode-014-browse-kill-ring)

;;; 02-mode-014-browse-kill-ring.el ends here
