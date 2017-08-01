;;; 02-mode-999-diminish.el --- configuration of diminish mode

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
;; [SUBHEADER.shrink major and minor mode name in the modeline]

;;; Change Log:
;; 2017-07-25 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; must be load after all other modes
(when (try-require 'diminish "    ")
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode " Ab"))
  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode " Y"))
  (eval-after-load "gtags"
    '(diminish 'gtags-mode " G"))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode " UndoT"))
  (eval-after-load "projectile-mode"
    '(diminish 'undo-tree-mode " Prj"))
  (eval-after-load "company-mode"
    '(diminish 'undo-tree-mode " C"))

  (add-hook 'emacs-lisp-mode-hook
    (lambda()
      (setq mode-name "el")))
  )


(provide '02-mode-999-diminish)

;;; 02-mode-999-diminish.el ends here
