;;; 00-environment-00-package.el --- a config file for package management -*- lexical-binding: t -*-

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.package management]
;; [SUBDEFAULT.nil]


;;; Code:
;;
;;; ELPA
(when (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
  ;; [VARCOMMENT.PROXY: proxy setting about package management]
  ;; [VARIABLE.tqnr-profile-environment-package-proxy-http nil]
  ;; [VARIABLE.tqnr-profile-environment-package-proxy-https nil]
  (when tqnr-profile-environment-package-proxy-http
    ;; set proxy
    (require 'url) ; need to have url-proxy-services defined
    ;; backquot instead of quote will allow use of comma to evaluate symbol after
    ;; example:
    ;;  (setq toto 12)
    ;;  (setq list '(("tata" . 42)))
    ;;  (add-to-list 'list `("titi" . ,toto))
    ;; result into a list (("titi" . 12) ("tata" . 42))
    (add-to-list 'url-proxy-services '("no_proxy" . "localhost") t)
    (add-to-list 'url-proxy-services `("http" . ,tqnr-profile-environment-package-proxy-http) t)
    (add-to-list 'url-proxy-services `("https" . ,tqnr-profile-environment-package-proxy-https) t))

  (when (try-require 'package "      ")
    (when tqnr-profile-environment-package-local-path
      (setq package-archives nil)
      ;; [VARCOMMENT.LOCAL PATH: proxy setting about package management]
      ;; [VARIABLE.tqnr-profile-environment-package-local-path nil]
      (add-to-list 'package-archives `("elpa" . ,(concat (file-name-as-directory tqnr-profile-environment-package-local-path) "elpa")))
      (add-to-list 'package-archives `("melpa" . ,(concat (file-name-as-directory tqnr-profile-environment-package-local-path) "melpa")))
      (add-to-list 'package-archives `("org" . ,(concat (file-name-as-directory tqnr-profile-environment-package-local-path) "org")))
      (add-to-list 'package-archives `("emacswiki" . ,(concat (file-name-as-directory tqnr-profile-environment-package-local-path) "emacswiki")))
      ) ;; (when tqnr-profile-environment-package-local-path
    ;; set path where put all packages
    (setq package-user-dir (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/elpa"))
    ;; init package
    (package-initialize)
    ;; refresh package list only when no package were found
    (when (not package-archive-contents)
      (package-refresh-contents))
    (when (not tqnr-section-mode-use-package)
      ;; install package from list defined in profile
      (mapc #'(lambda (package)
                (unless (package-installed-p package)
                  (package-install package)))
        ;; [VARCOMMENT.PACKAGE LIST: list of package like "'(first-package second-package)" to be installed]
        ;; [VARIABLE.tqnr-profile-environment-package-list '()]
        tqnr-profile-environment-package-list)
      )
    )
  )


(provide '00-environment-00-package)

;;; 00-environment-00-package.el ends here
