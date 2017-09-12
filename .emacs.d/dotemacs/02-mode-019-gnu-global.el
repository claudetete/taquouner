;;; 02-mode-019-gnu-global.el --- configuration of gnu global mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [SUBHEADER.Tag management mode]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-09-11 (0.2)
;;    remove autoload for performance
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; [VARCOMMENT.gtags interface (use modified gtags.el)]
;; [VARIABLE.tqnr-section-mode-gnu-global-gtags nil]
(if tqnr-section-mode-gnu-global-gtags
  (when (try-require 'autoload-gtags "      ")
    (autoload 'gtags-mode "gtags" "" t)
    (defun gtags-c-mode ()
      (gtags-mode 1)
      (setq gtags-select-buffer-single t))
    (gtags-mode t) ; only for diminish mode
    (add-hook 'c-mode-common-hook 'gtags-c-mode)
    (add-hook 'emacs-lisp-mode-hook 'gtags-c-mode))

  ;; [VARCOMMENT.ggtags interface]
  ;; [VARIABLE.tqnr-section-mode-gnu-global-ggtags nil]
  (when tqnr-section-mode-gnu-global-ggtags
    (when (try-require 'autoload-ggtags "      ")
      (eval-after-load "ggtags"
        '(setq ggtags-mode-line-project-name ""))
      (if tqnr-section-mode-helm
        (when (try-require 'helm-gtags "        ")
          (add-hook 'c-mode-hook 'helm-gtags-mode))
        (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))))
    )
  )



;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (if tqnr-section-mode-cedet-ecb
      (progn
        (define-key gtags-select-mode-map       (kbd "RET")             (lambda ()
                                                                          (interactive)
                                                                          (gtags-select-tag)
                                                                          (ecb-toggle-compile)
                                                                          ))
        (define-key gtags-select-mode-map       (kbd "<kp-enter>")      (lambda ()
                                                                          (interactive)
                                                                          (gtags-select-tag)
                                                                          (ecb-toggle-compile)
                                                                          ))
        ) ;; (progn ;; (if tqnr-section-mode-cedet-ecb

      (if tqnr-section-mode-helm ;; else tqnr-section-mode-cedet-ecb
        (with-eval-after-load "helm-gtags"
          (define-key helm-gtags-mode-map       (kbd "C-M-.")   'helm-gtags-find-pattern)
          (define-key helm-gtags-mode-map       (kbd "M-.")     'helm-gtags-dwim)
          (define-key helm-gtags-mode-map       (kbd "C->")     'helm-gtags-dwim)
          (define-key helm-gtags-mode-map       (kbd "C-<")     'helm-gtags-pop-stack)
          (define-key helm-gtags-mode-map       (kbd "C-.")     'helm-gtags-find-rtag)
          (define-key helm-gtags-mode-map       (kbd "C-M-,")   'helm-gtags-update-tags)
          (define-key helm-gtags-mode-map       (kbd "<f3>")    'helm-gtags-next-history)
          )

        (progn ;; else tqnr-section-mode-helm
          ;; find tag
          (global-set-key       (kbd "M-.")                     'ggtags-find-tag-dwim)
          (global-set-key       (kbd "C->")                     'ggtags-find-tag-dwim)
          (global-set-key       (kbd "M->")                     'semantic-goto-definition)

          ;; go back after find tag
          (global-set-key       (kbd "M-*")                     'ggtags-navigation-mode-abort)
          (global-set-key       (kbd "M-<kp-multiply>")         'ggtags-navigation-mode-abort)
          (global-set-key       (kbd "C-<")                     'ggtags-navigation-mode-abort)
          (global-set-key       (kbd "M-<")                     'semantic-pop-tag-mark)

          ;; find all references (regexp)
          (global-set-key       (kbd "C-M-.")                   'ggtags-find-reference)
          ) ;; (progn ;; else tqnr-section-mode-helm
        ) ;; (if tqnr-section-mode-helm ;; else tqnr-section-mode-cedet-ecb
      ) ;; (if tqnr-section-mode-cedet-ecb

    ;; find all references (regexp)
    (global-set-key     (kbd "C-M-=")           'gtags-find-with-grep-symbol-assigned)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-019-gnu-global)

;;; 02-mode-019-gnu-global.el ends here
