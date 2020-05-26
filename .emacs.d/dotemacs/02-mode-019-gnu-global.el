;;; 02-mode-019-gnu-global.el --- configuration of gnu global mode -*- lexical-binding: t -*-

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
;; Version: 0.4
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Tag management mode]
;; [SUBDEFAULT.nil]


;;; Code:
;; [VARCOMMENT.ggtags interface]
;; [VARIABLE.tqnr-section-mode-gnu-global-ggtags nil]
(use-package ggtags
  :bind
  ;; find all references (regexp)
  ("C-M-=" . gtags-find-with-grep-symbol-assigned)

  :config
  (setq ggtags-mode-line-project-name "")
  )

(if tqnr-section-mode-helm
  (use-package helm-gtags
    :bind (:map helm-gtags-mode-map
            ("C-M-." . helm-gtags-find-pattern)
            ("M-."   . helm-gtags-dwim)
            ("C->"   . helm-gtags-dwim)
            ("C-<"   . helm-gtags-pop-stack)
            ("C-."   . helm-gtags-find-rtag)
            ("C-M-," . helm-gtags-update-tags)
            ("<f3>"  . helm-gtags-next-history)
            )

    :hook
    (c-mode        . helm-gtags-mode)
    (ada-mode      . helm-gtags-mode)
    (c-mode-common .
      (lambda ()
        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
          (ggtags-mode 1)))))

  (use-package ggtags
    :bind
    ;; find tag
    ("M-." . ggtags-find-tag-dwim)
    ("C->" . ggtags-find-tag-dwim)
    ("M->" . semantic-goto-definition)

    ;; go back after find tag
    ("M-*"             . ggtags-navigation-mode-abort)
    ("M-<kp-multiply>" . ggtags-navigation-mode-abort)
    ("C-<"             . ggtags-navigation-mode-abort)
    ("M-<"             . semantic-pop-tag-mark)

    ;; find all references (regexp)
    ("C-M-." . ggtags-find-reference))
  ) ;; (if tqnr-section-mode-helm


(provide '02-mode-019-gnu-global)

;;; 02-mode-019-gnu-global.el ends here
