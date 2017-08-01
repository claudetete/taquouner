;;; 02-mode-052-ace-jump.el --- configuration of ace jump mode

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
;; [[SUBHEADER.move quickly and easily with ace jump
;; see http://dl.dropbox.com/u/3254819/AceJumpModeDemo/AceJumpDemo.htm
;; ]]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
;; to enable jump back
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
;; to enable only in the current window
(eval-after-load "ace-jump-mode" '(setq ace-jump-mode-scope 'window))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (global-set-key     (kbd "<f12>")           'ace-jump-mode)
    ;; can also use <C-u f9>
    (global-set-key     (kbd "<M-f12>")         'ace-jump-char-mode)
    ;; can also use <C-u C-u f9>
    (global-set-key     (kbd "<C-f12>")         'ace-jump-line-mode)
    (global-set-key     (kbd "<S-f12>")         'ace-jump-mode-pop-mark)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-052-ace-jump)

;;; 02-mode-052-ace-jump.el ends here
