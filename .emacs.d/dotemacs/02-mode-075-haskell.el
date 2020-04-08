;;; 02-mode-075-haskell.el --- configuration of haskell mode -*- lexical-binding: t -*-

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
;; [SUBHEADER.editing, debugging and developing Haskell programs]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package haskell-mode
  :bind (:map
          ("<S-f10>" . haskell-process-load-or-reload)
          ("<M-f10>" . haskell-interactive-bring)
          ;; ("C-c C-t" . haskell-process-do-type)
          ;; ("C-c C-i" . haskell-process-do-info)
          ;; ("C-c C-c" . haskell-process-cabal-build)
          ;; ("C-c C-k" . haskell-interactive-mode-clear)
          ;; ("C-c c"   . haskell-process-cabal)
          )

  :config
  ;; enable interactive mode with a prompt
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  :custom
  ;; remove suggest import line from ghc output
  (haskell-process-suggest-remove-import-lines t)
  ;; autoload module that have already been loaded
  (haskell-process-auto-import-loaded-modules t)
  ;; enable debug log
  (haskell-process-log t)
  ) ;; (use-package haskell-mode


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
