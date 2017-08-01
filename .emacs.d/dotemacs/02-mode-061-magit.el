;;; 02-mode-061-magit.el --- configuration of magit mode

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
;; [SUBHEADER.use git with nice interface (do not use vc interface from emacs)]
;;
;; FIXME update to newer version to work with emacs 25

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; under windows you can use msys make (after edit of Makefile) to install magit
;; dash is a dependency of projectile
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/dash.el-master"))
(when (try-require 'dash "    ")
  (add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/magit"))
  (when (try-require 'magit "    ")
    ;; [VARCOMMENT.path to git executable]
    ;; [VARIABLE.tqnr-profile-magit-exec "git"]
    (setq magit-git-executable tqnr-profile-magit-exec)
    (setq magit-commit-all-when-nothing-staged t)))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; git status (entry point of magit)
    (global-set-key     (kbd "C-c g g")         'magit-status)
    ;; git pull
    (global-set-key     (kbd "C-c g p")         'magit-pull)
    ;; git log
    (global-set-key     (kbd "C-c g l")         'magit-log)
    ;; git standard output
    (global-set-key     (kbd "C-c g d")         (lambda ()
                                                  (interactive)
                                                  (magit-display-process)
                                                  (switch-to-buffer "*magit-process*")))
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-061-magit)

;;; 02-mode-061-magit.el ends here
