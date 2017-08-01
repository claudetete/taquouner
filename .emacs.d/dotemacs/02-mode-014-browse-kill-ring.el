;;; 02-mode-014-browse-kill-ring.el --- configuration of browse kill ring mode

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
;; [SUBHEADER.mode to browse the kill ring memory yank only on the first left top window...]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'autoload-browse-kill-ring "    ")
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


(add-hook 'browse-kill-ring-hook
  (lambda ()
    ;; move next and previous with arrow
    (local-set-key      (kbd "<up>")            'browse-kill-ring-previous)
    (local-set-key      (kbd "<down>")          'browse-kill-ring-forward)
    ;; quit not only with q but also with C-g or F2 with ecb
    (local-set-key      (kbd "C-g")             'browse-kill-ring-quit)
    (when tqnr-section-mode-cedet-ecb
      (local-set-key    (kbd "<f2>")            'browse-kill-ring-quit))
    (local-set-key      (kbd "<escape>")        'browse-kill-ring-quit)
    (local-set-key      (kbd "<return>")        'browse-kill-ring-insert-move-and-quit)
    )
  )


(provide '02-mode-014-browse-kill-ring)

;;; 02-mode-014-browse-kill-ring.el ends here
