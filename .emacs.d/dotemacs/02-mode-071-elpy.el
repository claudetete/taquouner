;;; 02-mode-071-elpy.el --- configuration of elpy mode

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
;; [[SUBHEADER.Python mode like an IDE (only install is from package)
;;  ;; add elpy package
;;  ;; and flycheck package, about warnings/errors check on the fly
;;  ;; and autopep8 package, about fix automagically some pep8 rules after save python file
;;  (add-to-list 'profile-environment-elpa-package-list 'elpy t)
;;  (add-to-list 'profile-environment-elpa-package-list 'flycheck t)
;;  (add-to-list 'profile-environment-elpa-package-list 'py-autopep8 t)
;; ]]

;;; Change Log:
;; 2017-07-25 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; must be install from list of packages
(elpy-enable)
(elpy-use-ipython)

;;(setq python-shell-interpreter "C:/WinPython27/python-2.7.10/Scripts/ipython.exe")
;;(setq python-shell-interpreter-args "")
;;(setq python-shell-interpreter-args "-i")

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;;(require 'py-autopep8)
;;(setq py-autopep8-options '("--ignore=E22,E224,E501"))
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; found at https://github.com/jorgenschaefer/elpy/issues/733
(setq python-shell-unbuffered nil)

;; add customize compile command line to execute current python file
;; found at http://stackoverflow.com/questions/12756531/using-the-current-buffers-file-name-in-m-x-compile
(add-hook 'elpy-mode-hook
  (lambda ()
    (set (make-local-variable 'compile-command)
      (concat "python " (shell-quote-argument buffer-file-name)))))


(add-hook 'elpy-mode-hook
  (lambda ()
    (local-unset-key            (kbd "M-TAB"))
    (define-key elpy-mode-map   (kbd "<C-M-prior>")     'backward-page)
    (define-key elpy-mode-map   (kbd "<C-M-next>")      'forward-page)
    ;; remap C-up/down to move from empty line to empty line (like any other mode)
    (define-key elpy-mode-map   (kbd "<C-up>")          'backward-paragraph)
    (define-key elpy-mode-map   (kbd "<C-down>")        'forward-paragraph)
    ;; remap M-up/down to move from block to block (useful because python do
    ;; not use {} for block so cannot use the trick about go to matched
    ;; keyword/character)
    (define-key elpy-mode-map   (kbd "<M-up>")          'elpy-nav-backward-block)
    (define-key elpy-mode-map   (kbd "<M-down>")        'elpy-nav-forward-block)
    )
  )


(provide '02-mode-071-elpy)

;;; 02-mode-071-elpy.el ends here
