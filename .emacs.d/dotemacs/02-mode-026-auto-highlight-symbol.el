;;; 02-mode-026-auto-highlight-symbol.el --- configuration of auytohighlight symbol mode

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
;; [SUBHEADER.to automatically highlight symbol at point]
;; [SUBDEFAULT.t]


;;; Code:
;; after some idle time the symbol at point will be highlighted in display area
;; autoload file are generate by autoload.el
(when (try-require 'autoload-auto-highlight-symbol "    ")
  ;; active the mode
  (global-auto-highlight-symbol-mode t)
  (custom-set-variables
    ;; do not ignore case
    '(ahs-case-fold-search nil)
    ;; increase idle time to display highlight
    '(ahs-idle-interval 2.2)
    )
  )


(provide '02-mode-026-auto-highlight-symbol)

;;; 02-mode-026-auto-highlight-symbol.el ends here
