;;; 02-mode-067-projectile.el --- configuration of projectile mode

;; Copyright (c) 2017-2018 Claude Tete
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
;; Last-Updated: January 2018

;;; Commentary:
;;
;; [SUBHEADER.Project management, filtered find-file, only with root file from version control]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2018-01-31 (0.2)
;;    modify projectile git command to include submodules files + use
;;    .projectile file in git root project
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; dash is a dependency of projectile
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/dash.el-master"))
(when (try-require 'dash "    ")
  ;; list all files even in git submodules (remove --others and add --recurse-submodules)
  (custom-set-variables
    '(projectile-git-command "git ls-files -z --cached --recurse-submodules --exclude-standard"))
  ;; dash is found so load projectile
  (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/projectile-master"))
  ;; (eval-after-load "projectile"
  ;;   '(setq projectile-mode-line
  ;;      '(:eval (list " ["
  ;;                (propertize (projectile-project-name)
  ;;                  ;; color of solarized light
  ;;                  ;; 'face '(:foreground "#8100be" :background "#93a1a1"))
  ;;                  'face 'powerline-active1)
  ;;                "]"))))
  (eval-after-load "projectile"
    '(setq projectile-mode-line-function '(lambda () (format " [%s]" (propertize (projectile-project-name)
                                                                       'face 'mode-line)))))
  ;; (defun projectile-mode-line-function-custom ()
  ;;   (when (ignore-errors (projectile-project-root))
  ;;     '(:eval (list " ["
  ;;              (propertize (projectile-project-name)
  ;;                ;; color of solarized light
  ;;                ;; 'face '(:foreground "#8100be" :background "#93a1a1"))
  ;;                'face 'powerline-active1)
  ;;              "]"))))
  ;; (eval-after-load "projectile"
  ;;   '(setq projectile-mode-line-function 'projectile-mode-line-function-custom)
  ;;   )
  (when (try-require 'autoload-projectile "      ")
    ;; enable projectile
    (projectile-global-mode)
    ;; add prefix shortcut to C-c p
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ;; add find of Synergy project
    ;;(push "_ccmwaid.inf" projectile-project-root-files-bottom-up)
    ;; use external files indexer (old method)
    (setq projectile-indexing-method 'hybrid)
    ;; enable cache of files index
    (setq projectile-enable-caching t)
    ;; remove "Projectile" in mode-line from "Projectile[myProject]" to "[MyProject]"
    ;;(setq projectile-mode-line (format " [%s]" (projectile-project-name)))
    ;; use helm with projectile
    (when tqnr-section-mode-helm
      (when (try-require 'autoload-helm-projectile "          ")
        (setq projectile-completion-system 'helm)

        (helm-projectile-on))
      )

    ;; ignore GNU Global file with projectile
    (when tqnr-section-mode-gnu-global
      (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
        (add-to-list 'projectile-globally-ignored-files item))
      )
    )
  )


(when tqnr-section-mode-helm
  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; find file using projectile and helm (override find-name-dired in 08-shortcut-03-grep)
      (global-set-key   (kbd "C-c C-f")         'helm-projectile-find-file)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )



(provide '02-mode-067-projectile)

;;; 02-mode-067-projectile.el ends here
