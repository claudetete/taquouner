;;; 02-mode-023-clearcase.el --- configuration of clearcase mode

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
;; [SUBHEADER.ClearCase mode]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:

(setq clearcase-mode nil)
;; toggle compile window when quit config spec mode
(when tqnr-section-mode-cedet-ecb
  (add-hook 'clearcase-config-spec-quit-hook 'ecb-toggle-compile))

;; [VARCOMMENT.ClearCase Emacs integration]
;; [VARIABLE.tqnr-section-mode-clearcase-el nil]
(when tqnr-section-mode-clearcase-el
  (try-require 'clearcase "    ")

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; ediff with previous (can open a new frame before with C-x 5 2)
      (global-set-key   (kbd "C-x v `")         'clearcase-ediff-pred-current-buffer)
      ;; ediff with named version
      (global-set-key   (kbd "C-x v 1")         'clearcase-ediff-named-version-current-buffer)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )


(provide '02-mode-023-clearcase)

;;; 02-mode-023-clearcase.el ends here
