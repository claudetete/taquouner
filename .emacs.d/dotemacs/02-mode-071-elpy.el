;;; 02-mode-071-elpy.el --- configuration of elpy mode -*- lexical-binding: t -*-

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [[SUBHEADER.Python mode like an IDE (only install is from package)
;;  ;; add elpy package
;;  ;; and flycheck package, about warnings/errors check on the fly
;;  ;; and autopep8 package, about fix automagically some pep8 rules after save python file
;;  (add-to-list 'profile-environment-elpa-package-list 'elpy t)
;;  (add-to-list 'profile-environment-elpa-package-list 'flycheck t)
;;  (add-to-list 'profile-environment-elpa-package-list 'py-autopep8 t)
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package elpy
  :defer t

  :init
  (elpy-enable)

  :config
  (local-unset-key            (kbd "M-TAB"))
  ;;(elpy-use-ipython)

  ;;(setq python-shell-interpreter "C:/WinPython27/python-2.7.10/Scripts/ipython.exe")
  ;;(setq python-shell-interpreter-args "")
  ;;(setq python-shell-interpreter-args "-i")

  ;; use flycheck not flymake with elpy
  (use-package flycheck
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))

    :hook
    (elpy-mode-hook . flycheck-mode))

  ;; enable autopep8 formatting on save
  ;;(require 'py-autopep8)
  ;;(setq py-autopep8-options '("--ignore=E22,E224,E501"))
  ;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

  ;; found at https://github.com/jorgenschaefer/elpy/issues/733
  (setq python-shell-unbuffered nil)

  ;; [VARCOMMENT.override path for created elpy virtualenv (should have rights to execute)]
  ;; [VARIABLE.tqnr-section-mode-elpy-rpc-virtualenv-path nil]
  (when tqnr-section-mode-elpy-rpc-virtualenv-path
    (setq elpy-rpc-virtualenv-path tqnr-section-mode-elpy-rpc-virtualenv-path))

  ;; add customize compile command line to execute current python file
  ;; found at http://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile
  :hook
  (elpy-mode-hook .
    (lambda ()
      (set (make-local-variable 'compile-command)
        (concat "python " (shell-quote-argument buffer-file-name)))))

  :bind (:map elpy-mode-map
          ("<C-M-prior>" . backward-page)
          ("<C-M-next>"  . forward-page)
          ;; remap C-up/down to move from empty line to empty line (like any other mode)
          ("<C-up>"      . backward-paragraph)
          ("<C-down>"    . forward-paragraph)
          ;; remap M-up/down to move from block to block (useful because python do
          ;; not use {} for block so cannot use the trick about go to matched
          ;; keyword/character)
          ("<M-up>"      . elpy-nav-backward-block)
          ("<M-down>"    . elpy-nav-forward-block)
          )
  ) ;; (use-package elpy


(provide '02-mode-071-elpy)

;;; 02-mode-071-elpy.el ends here
