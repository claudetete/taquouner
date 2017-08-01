;;; 02-mode-068-company.el --- configuration of company mode

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
;; [SUBHEADER.Completion mode using external back-ends to have symbol]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'company "    ")
  (setq company-backends (delete 'company-semantic company-backends))
  (add-hook 'after-init-hook 'global-company-mode)
  (when tqnr-section-mode-helm
    (try-require 'autoload-helm-company "      "))

  ;; when irony mode is used
  (when tqnr-section-mode-irony
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
    )


  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      (if tqnr-section-mode-helm
        (eval-after-load 'company
          '(progn
             ;; use helm with company mode
             (define-key company-mode-map     (kbd "C-/")     'helm-company)
             (define-key company-active-map   (kbd "C-/")     'helm-company)))
        ;; use company even when helm is not used
        (global-set-key     (kbd "C-/")             'company-complete))
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )


(provide '02-mode-068-company)

;;; 02-mode-068-company.el ends here
