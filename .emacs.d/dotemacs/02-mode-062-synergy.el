;;; 02-mode-062-synergy.el --- configuration of synergy mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.use synergy without java client GUI (do not use vc interface from emacs)]
;; [SUBDEFAULT.nil]


;;; Code:
(when (try-require 'autoload-synergy-web "    ")
  ;; [VARCOMMENT.login to connect to synergy server]
  ;; [VARIABLE.tqnr-profile-synergy-username ""]
  (setq synergy-username                tqnr-profile-synergy-username)
  ;; [VARCOMMENT.database path to connect to synergy server]
  ;; [VARIABLE.tqnr-profile-synergy-database ""]
  (setq synergy-database                tqnr-profile-synergy-database)
  ;; [VARCOMMENT.server url to connect to synergy server]
  ;; [VARIABLE.tqnr-profile-synergy-server ""]
  (setq synergy-server                  tqnr-profile-synergy-server)
  ;; [[VARCOMMENT.command line to modify history output
  ;; by example: '("|" "sed" "s/login/readable_name/")
  ;; ]]
  ;; [VARIABLE.tqnr-profile-synergy-history-filter '()]
  (setq synergy-history-filter          tqnr-profile-synergy-history-filter)
  ;; [VARCOMMENT.external tool to do diff with synergy]
  ;; [VARIABLE.tqnr-profile-synergy-diff-external-command ""]
  (setq synergy-diff-external-command   tqnr-profile-synergy-diff-external-command)
  ;; [VARCOMMENT.command line parameter to external tool to do diff]
  ;; [VARIABLE.tqnr-profile-synergy-diff-external-parameter ""]
  (setq synergy-diff-external-parameter tqnr-profile-synergy-diff-external-parameter)
  ;; [VARCOMMENT.swap files in diff (left/right) about external diff tool]
  ;; [VARIABLE.tqnr-profile-synergy-diff-external-swap-file nil]
  (setq synergy-diff-external-swap-file tqnr-profile-synergy-diff-external-swap-file))


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; start
    (global-set-key     (kbd "C-c s s")         'synergy-start)
    ;; checkout
    (global-set-key     (kbd "C-c s c")         'synergy-checkout)
    ;; checkout with current task
    (global-set-key     (kbd "C-c s x")         'synergy-checkout-with-current-task)
    ;; uncheckout
    (global-set-key     (kbd "C-c s d")         'synergy-uncheckout)
    ;; diff
    (global-set-key     (kbd "C-c s =")         'synergy-diff-with-previous)
    ;; history
    (global-set-key     (kbd "C-c s h")         'synergy-history)
    ;; reconcile to server
    (global-set-key     (kbd "C-c s r")         'synergy-reconcile-to-server)
    ;; reconcile from server
    (global-set-key     (kbd "C-c s w")         'synergy-reconcile-to-local)
    ;; show conflict
    (global-set-key     (kbd "C-c s f")         'synergy-show-conflict)
    ;; create task
    (global-set-key     (kbd "C-c s t c")       'synergy-create-task)
    ;; set current task
    (global-set-key     (kbd "C-c s t d")       'synergy-set-current-task)
    ;; set task release
    (global-set-key     (kbd "C-c s t r")       'synergy-set-task-as-current)
    ;; complete/checkin task
    (global-set-key     (kbd "C-c s t i")       'synergy-checkin-task)
    ;; update/reconfigure
    (global-set-key     (kbd "C-c s u")         'synergy-reconfigure)
    ;; update/reconfigure current
    (global-set-key     (kbd "C-c s p")         'synergy-reconfigure-current-project)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-062-synergy)

;;; 02-mode-062-synergy.el ends here
