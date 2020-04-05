;;; 02-mode-013-yasnippet.el --- configuration of yasnippet mode

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
;; [SUBHEADER.enable snippet for emacs]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package yasnippet
  :bind
  ("C-c C-e" 'yas/expand)

  :init
  (yas/global-mode 1)

  :config
  ;; Add your own snippets to ~/.emacs.d/plugins/snippets by placing files there or invoking yas-new-snippet.
  (setq yas-snippet-dirs
    '(
       ;; keep default value
       "~/.emacs.d/snippets"
       ;; add path to collection of snippets
       (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/snippets")
       ))

  ) ;; (use-package yasnippet


(provide '02-mode-013-yasnippet)

;;; 02-mode-013-yasnippet.el ends here
