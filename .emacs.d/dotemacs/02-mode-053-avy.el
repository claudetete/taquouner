;;; 02-mode-053-avy.el --- configuration of avy mode

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.move quickly and easily with avy (replacement of ace jump)]
;; [SUBDEFAULT.t]


;;; Code:
(use-package avy
  :bind
  ("<f12> <f12>"  . avy-goto-char-2)
  ("<f12> <up>"   . avy-goto-char-2-above)
  ("<f12> <down>" . avy-goto-char-2-below)
  ("<S-F12>"      . avy-pop-mark)
  ;; move to line and keep column position
  ;; workaround from https://github.com/abo-abo/avy/issues/127
  ("<C-f12> <C-f12>" . (lambda ()
                         (interactive)
                         (let ((col (current-column)))
                           (avy-goto-line)
                           (move-to-column col))))
  ("<C-f12> <up>" . (lambda ()
                      (interactive)
                      (let ((col (current-column)))
                        (avy-goto-line-above)
                        (move-to-column col))))
  ("<C-f12> <down>" . (lambda ()
                        (interactive)
                        (let ((col (current-column)))
                          (avy-goto-line-below)
                          (move-to-column col))))

  :config
  ;; do search only in current buffer
  (setq avy-all-windows nil)
  ) ;; (use-package avy


(use-package avy-goto-word-2
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/avy-goto-word-2.el"))

  :bind
  ("<M-f12> <M-f12>" . avy-goto-word-2)
  ("<M-f12> <up>"    . avy-goto-word-2-above)
  ("<M-f12> <down>"  . avy-goto-word-2-below)
  ) ;; (use-package avy-goto-word-2


(provide '02-mode-053-avy)

;;; 02-mode-053-avy.el ends here
