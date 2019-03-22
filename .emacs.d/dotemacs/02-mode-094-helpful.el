;;; 02-mode-094-helpful.el --- configuration of helpful mode

;; Copyright (c) 2018-2019 Claude Tete
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
;; Created: December 2018
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.Helpful mode]
;; [SUBDEFAULT.nil]


;;; Code:

(when (try-require 'helpful "    ")
  ;; add some source code example to helpful
  (when (try-require 'elisp-demos "      ")
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
    )

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; Note that the built-in `describe-function' includes both functions
      ;; and macros. `helpful-function' is functions only, so we provide
      ;; `helpful-callable' as a drop-in replacement.
      (global-set-key (kbd "C-h f") #'helpful-callable)

      (global-set-key (kbd "C-h v") #'helpful-variable)
      (global-set-key (kbd "C-h k") #'helpful-key)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )

(provide '02-mode-094-helpful)

;;; 02-mode-094-helpful.el ends here
