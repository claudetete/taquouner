;;; 02-mode-075-haskell.el --- configuration of haskell mode

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
;; [SUBHEADER.editing, debugging and developing Haskell programs]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-07-25 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/haskell-mode/"))
(when (try-require 'haskell-mode-autoloads "    ")
  ;; add info/help in emacs
  (add-to-list 'Info-default-directory-list (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/haskell-mode/"))
  ;; enable interactive mode with a prompt
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (custom-set-variables
    ;; remove suggest import line from ghc output
    '(haskell-process-suggest-remove-import-lines t)
    ;; autoload module that have already been loaded
    '(haskell-process-auto-import-loaded-modules t)
    ;; enable debug log
    '(haskell-process-log t)
    )
  )


(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map       (kbd "<S-f10>")         'haskell-process-load-or-reload)
     (define-key haskell-mode-map       (kbd "<M-f10>")         'haskell-interactive-bring)
     ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     ;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     ;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     ;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
     )
  )


(provide '02-mode-075-haskell)

;;; 02-mode-075-haskell.el ends here
