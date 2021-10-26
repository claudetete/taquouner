;;; 02-mode-000-use-package.el --- configuration of use-package -*- lexical-binding: t -*-

;; Copyright (c) 2020 Claude Tete
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
;; Version: 0.1
;; Created: April 2020
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Package configuration with simple and tidy macro (mandatory)]
;; [SUBDEFAULT.t]


;;; Code:
(when tqnr-section-environment-package
  (setq package-list '(use-package))

  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
    package-list)
    
  (when (try-require 'use-package-ensure "      ")
    (setq use-package-always-ensure t))
    
  ;; Asynchronous processing
  ;; settings about too much parallel async process on MS Windows
  ;; steal from https://gist.github.com/kiennq/cfe57671bab3300d3ed849a7cbf2927c
  (use-package async
   :init
   (setq async-bytecomp-allowed-packages '(all))
   
   :config
   (async-bytecomp-package-mode 1)
   (dired-async-mode 1)
   ;; limit number of async processes
   (eval-when-compile
     (require 'cl-lib))
   (defvar async-maximum-parallel-procs 4)
   (defvar async--parallel-procs 0)
   (defvar async--queue nil)
   (defvar-local async--cb nil)
   (advice-add #'async-start :around
               (lambda (orig-func func &optional callback)
                 (if (>= async--parallel-procs async-maximum-parallel-procs)
                     (push `(,func ,callback) async--queue)
                   (cl-incf async--parallel-procs)
                   (let ((future (funcall orig-func func
                                          (lambda (re)
                                            (cl-decf async--parallel-procs)
                                            (when async--cb (funcall async--cb re))
                                            (when-let (args (pop async--queue))
                                              (apply #'async-start args))))))
                     (with-current-buffer (process-buffer future)
                       (setq async--cb callback)))))
               '((name . --queue-dispatch)))
    )
  )

    
(provide '02-mode-000-use-package)

;;; 02-mode-000-use-package.el ends here
