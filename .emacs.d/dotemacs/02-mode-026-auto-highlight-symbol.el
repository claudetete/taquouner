;;; 02-mode-026-auto-highlight-symbol.el --- configuration of auytohighlight symbol mode

;; Copyright (c) 2017-2020 Claude Tete
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
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.to automatically highlight symbol at point]
;; [SUBDEFAULT.t]


;;; Code:

;; try highlight-thing
;; (use-package highlight-thing
;;   :init
;;   (global-highlight-thing-mode)

;;   :config
;;   ;; highlight only words
;;   (setq highlight-thing-what-thing 'word)
;;   ;; increase idle time to display highlight
;;   (setq highlight-thing-delay-seconds 2.2)
;;   ;; case-sensitive
;;   (setq highlight-thing-case-sensitive-p t)
;;   ;; highlight a given thing only if it is 15 lines above or below point when
;;   ;; in a buffer that has more than 5000 "characters", no such restriction
;;   ;; would apply to buffers with less characters
;;   (setq highlight-thing-limit-to-region-in-large-buffers-p t
;;         highlight-thing-narrow-region-lines 35
;;         highlight-thing-large-buffer-limit 5000)
;;   )

;; after some idle time the symbol at point will be highlighted in display area
;; autoload file are generate by autoload.el
(use-package auto-highlight-symbol
  :init
  ;; active the mode
  (global-auto-highlight-symbol-mode t)

  :custom
  ;; do not ignore case
  (ahs-case-fold-search nil)
  ;; increase idle time to display highlight
  (ahs-idle-interval 2.2)
  ) ;; (use-package auto-highlight-symbol


(provide '02-mode-026-auto-highlight-symbol)

;;; 02-mode-026-auto-highlight-symbol.el ends here
