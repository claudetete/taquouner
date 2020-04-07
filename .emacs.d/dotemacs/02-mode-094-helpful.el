;;; 02-mode-094-helpful.el --- configuration of helpful mode

;; Copyright (c) 2018-2020 Claude Tete
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
;; Created: December 2018
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Helpful mode]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)

  :config
  ;; add some source code example to helpful
  (use-package elisp-demos
    :config
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
  ) ;; (use-package helpful


(provide '02-mode-094-helpful)

;;; 02-mode-094-helpful.el ends here
