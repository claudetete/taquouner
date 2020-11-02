;;; 02-mode-020-counsel-compile.el --- configuration of counsel-compile mode -*- lexical-binding: t -*-

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
;; Created: May 2020
;; Last-Updated: May 2020

;;; Commentary:
;;
;; [SUBHEADER.Use ivy to select a compile command.]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package counsel-compile
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/counsel-compile.el"))

  :bind
  ("<f10>" . counsel-compile)

  :config
  (when tqnr-section-mode-projectile-direnv
    ;; Use env variable to get compile command
    (setq counsel-compile-sources
      (lambda ()
        (let ((commands (getenv "COMPILE_LIST")))
          (if commands
            (s-split "," commands t)
            '()))))
    ;; set prefix of each command
    (setq counsel-compile-command-prefixs
      (lambda ()
        (getenv "COMPILE_PREFIX")))
    ) ;; (when tqnr-section-mode-projectile-direnv
  ) ;; (use-package counsel-compile


(provide '02-mode-020-counsel-compile)

;;; 02-mode-020-counsel-compile.el ends here
