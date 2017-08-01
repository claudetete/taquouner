;;; 02-mode-018-dired-plus.el --- configuration of dired+ mode

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
;; [SUBHEADER.improve Dired mode, color, open with, etc]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:

;; (when (try-require 'autoload-dired+ "    ")
;;   ;; to have only one dired buffer by dired instance
;;   (toggle-diredp-find-file-reuse-dir t)
;;   )


(eval-after-load "dired"
  '(progn
     ;; open with default associated application
     (define-key dired-mode-map         (kbd "H-RET")   'dired-w32-browser)
     ;; was dired-advertised-find-file
     (define-key dired-mode-map         (kbd "RET")     'dired-find-alternate-file)
     ;; was dired-up-directory
     (define-key dired-mode-map (kbd "[") (lambda () (interactive) (find-alternate-file "..")))
     ;;(define-key dired-mode-map         (kbd "[")       'dired-up-directory)
     )
  )


(provide '02-mode-018-dired-plus)

;;; 02-mode-018-dired-plus.el ends here
