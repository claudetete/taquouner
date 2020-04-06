;;; 02-mode-082-shackle.el --- configuration of arduino mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Created: August 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [[SUBHEADER.mode to have popup always following same rules
;; like popwin but just add constraint to popup not replace the whole thing
;; Helm does not like popwin...
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(when (try-require 'shackle "    ")
  (when tqnr-section-mode-helm
    ;; make helm play nice with popup
    (setq helm-display-function 'pop-to-buffer)
    ;; rules to add helm
    (setq shackle-rules '(
                           ;; use 25% at bottom
                           ;; all helm buffer
                           ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.25)
                           ;; helm swoop buffer
                           ("\\`\\*Helm.*?\\*\\'" :regexp t :align t :size 0.25)
                           ))
    (when tqnr-section-mode-helm-swoop
      ;; to be properly displayed by shackle
      (setq helm-swoop-split-window-function 'display-buffer)
      )
    ) ;; (when tqnr-section-mode-helm
  ;; enable shackle mode
  (shackle-mode 1)
  ) ;; (when (try-require 'shackle "    ")


(provide '02-mode-082-shackle)

;;; 02-mode-082-shackle.el ends here
