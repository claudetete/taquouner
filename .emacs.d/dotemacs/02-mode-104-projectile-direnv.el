;;; 02-mode-104-projectile-direnv.el --- configuration of projectile-direnv -*- lexical-binding: t -*-

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
;; [SUBHEADER.Projectile direnv mode to set environment variable as found in direnv config]
;; [SUBDEFAULT.nil]


;;; Code:
(when tqnr-section-mode-projectile
  (use-package projectile-direnv
    :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/projectile-direnv.el"))

    :config
    (projectile-direnv-global-mode t)
    (setq projectile-direnv-make-local t)
    (setq projectile-direnv-exec-path-mandatory exec-path-init)
    (setq projectile-direnv-compile-env t)

    ) ;; (use-package projectile-direnv
  ) ;; (when tqnr-section-mode-projectile


(provide '02-mode-104-projectile-direnv)

;;; 02-mode-104-projectile-direnv.el ends here
