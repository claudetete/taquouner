;;; 02-mode-067-projectile.el --- configuration of projectile mode

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.Project management, filtered find-file, only with root file from version control]
;; [SUBDEFAULT.t]


;;; Code:
(use-package projectile
  :bind
  ;; find file using projectile and helm (override find-name-dired in 08-shortcut-03-grep)
  ("C-c C-f" . helm-projectile-find-file)

  :init
  ;; enable projectile
  (projectile-global-mode)

  :config
  (setq projectile-mode-line-function '(lambda () (format " [%s]" (propertize (projectile-project-name)
                                                                    'face 'mode-line))))
  (when tqnr-section-mode-synergy
    ;; add find of Synergy project
    (push "_ccmwaid.inf" projectile-project-root-files-bottom-up))
  ;; use external files indexer (old method)
  (setq projectile-indexing-method 'hybrid)
  ;; enable cache of files index
  (setq projectile-enable-caching t)
  ;; ignore GNU Global file with projectile
  (when tqnr-section-mode-gnu-global
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))
    )
  ;; use helm with projectile
  (when tqnr-section-mode-helm
    (use-package helm-projectile
      :init
      (helm-projectile-on)

      :config
      (setq projectile-completion-system 'helm)))

  :custom
  ;; list all files even in git submodules (remove --others and add --recurse-submodules)
  (projectile-git-command "git ls-files -z --cached --recurse-submodules --exclude-standard")

  :bind (:map projectile-mode-map
          ;; add prefix shortcut to C-c p
          ("C-c p" . projectile-command-map))


  ) ;; (use-package projectile


(provide '02-mode-067-projectile)

;;; 02-mode-067-projectile.el ends here
