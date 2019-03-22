;;; 02-mode-053-avy.el --- configuration of avy mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.move quickly and easily with avy (replacement of ace jump)]
;; [SUBDEFAULT.t]


;;; Code:
;;(mapc #'byte-compile-file '("avy.el"))
(try-require 'autoload-avy "    ")
;; do search only in current buffer
(eval-after-load "avy" '(setq avy-all-windows nil))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (global-set-key     (kbd "<f12> <f12>")     'avy-goto-char-2)
    (global-set-key     (kbd "<f12> <up>")      'avy-goto-char-2-above)
    (global-set-key     (kbd "<f12> <down>")    'avy-goto-char-2-below)
    (global-set-key     (kbd "<M-f12> <M-f12>") 'avy-goto-word-2)
    (global-set-key     (kbd "<M-f12> <up>")    'avy-goto-word-2-above)
    (global-set-key     (kbd "<M-f12> <down>")  'avy-goto-word-2-below)
    ;; move to line and keep column position
    ;; workaround from https://github.com/abo-abo/avy/issues/127
    (global-set-key     (kbd "<C-f12> <C-f12>") (lambda ()
                                                  (interactive)
                                                  (let ((col (current-column)))
                                                    (avy-goto-line)
                                                    (move-to-column col))))
    (global-set-key     (kbd "<C-f12> <up>")    (lambda ()
                                                  (interactive)
                                                  (let ((col (current-column)))
                                                    (avy-goto-line-above)
                                                    (move-to-column col))))
    (global-set-key     (kbd "<C-f12> <down>")  (lambda ()
                                                  (interactive)
                                                  (let ((col (current-column)))
                                                    (avy-goto-line-below)
                                                    (move-to-column col))))

    (global-set-key     (kbd "<S-F12>")         'avy-pop-mark)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook



(provide '02-mode-053-avy)

;;; 02-mode-053-avy.el ends here
