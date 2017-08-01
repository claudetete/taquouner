;;; 00-environment-00-package.el --- a config file for package management

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
;; [SUBHEADER.package management]

;;; Change Log:
;; 2017-07-20 (0.1)
;;    creation from environment.el


;;; Code:
;;
;;; ELPA
(when (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
  ;; [VARCOMMENT.PROXY: proxy setting about package management]
  ;; [VARIABLE.tqnr-profile-environment-elpa-proxy-http nil]
  ;; [VARIABLE.tqnr-profile-environment-elpa-proxy-https nil]
  (when tqnr-profile-environment-elpa-proxy-http
    ;; add to load path the profile directory
    (require 'url) ; need to have url-proxy-services defined
    ;; backquot instead of quote will allow use of comma to evaluate symbol after
    ;; example:
    ;;  (setq toto 12)
    ;;  (setq list '(("tata" . 42)))
    ;;  (add-to-list 'list `("titi" . ,toto))
    ;; result into a list (("titi" . 12) ("tata" . 42))
    (add-to-list 'url-proxy-services '("no_proxy" . "localhost") t)
    (add-to-list 'url-proxy-services `("http" . ,tqnr-profile-environment-elpa-proxy-http) t)
    (add-to-list 'url-proxy-services `("https" . ,tqnr-profile-environment-elpa-proxy-https) t))

  (when (try-require 'package "    ")
    ;; set package server
    (add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/") t)
    (add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/") t)
    ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
    ;; set proxy
    ;; set path where put all packages
    (setq package-user-dir (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/elpa"))
    ;; init package
    (package-initialize)
      ;; refresh package list only when no package were found
    (when (not package-archive-contents)
      (package-refresh-contents))
    ;; install package from list defined in profile
    (mapc #'(lambda (package)
              (unless (package-installed-p package)
                (package-install package)))
      ;; [VARCOMMENT.PACKAGE LIST: list of package like "'(first-package second-package)" to be installed]
      ;; [VARIABLE.tqnr-profile-environment-elpa-package-list '()]
      tqnr-profile-environment-elpa-package-list)
    )
  )


(provide '00-environment-00-package)

;;; 00-environment-00-package.el ends here
