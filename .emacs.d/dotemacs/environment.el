;;; environment.el --- a config file for environment settings

;; Copyright (c) 2006-2016 Claude Tete
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

;; Keywords: config, environment, os, path
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 3.0
;; Created: October 2006
;; Last-Updated: September 2016

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment'
;;              var     `profile'
;;              var     `dotemacs-path'

;;; Change Log:
;; 2016-09-28 (3.0)
;;    add emacs version 25 into env variable + update package management
;; 2015-08-21 (2.9)
;;    add environment variable for emacs version 24.4 and 24.5
;; 2012-12-27 (2.8)
;;    update dot emacs path
;; 2012-10-26 (2.7)
;;    put start client server with configuration
;; 2012-07-09 (2.6)
;;    add robustness
;; 2012-06-12 (2.5)
;;    add version recognition + condition for packages
;; 2012-06-08 (2.4)
;;    add ediff path + add environment variable for cygwin error
;; 2012-06-05 (2.3)
;;    use profile instead of working environment and put all settings
;;    in profile files
;; 2012-05-28 (2.2)
;;    modify path for cedet for AT + disable isearch+ for default
;; 2012-05-25 (2.1)
;;    add path for msys + new mode for default working environment
;; 2012-05-11 (2.0)
;;    use color-theme in terminal + change path for AT + add hyper and super
;; 2012-05-04 (1.9)
;;    complete default working environment + change location to avoid French
;;    message
;; 2012-05-03 (1.8)
;;    change environment variable management
;; 2012-04-20 (1.7)
;;    add default environment
;; 2012-04-03 (1.6)
;;    add terminal environment to Alstom Transport
;; 2012-03-28 (1.5)
;;    translate comments in English
;; 2012-03-02 (1.4)
;;    add working environment and change "OS" message
;; 2012-03-02 (1.3)
;;    add/change/remove some path
;; 2011-07-09 (1.2)
;;    add running-in-graphical or terminal
;; 2011-04-21 (1.1)
;;    add running-on-ms-windows or gnu-linux
;; 2010-10-11 (1.0)
;;    add add path for ms windows
;; 2010-06-11 (0.1)
;;    creation from scratch + env test from other .emacs


;;; Code:
;;
;;; PROFILE
(when section-environment-profile (message "  0.1 Profile...")
  (try-require 'profile "    ")
  (message "  0.1 Profile...Done"))

;;
;;; VERSION RECOGNITION
(when section-environment-version-recognition (message "  0.2 Version Recognition...")
  (defvar running-on-emacs-23 nil)
  (defvar running-on-emacs-24 nil)
  (defvar running-on-emacs-25 nil)
  (defvar running-on-emacs-24-4 nil)
  (defvar running-on-emacs-24-5 nil)
  (cond
    ;; Emacs 23.x
    ((= emacs-major-version 23)
      (setq running-on-emacs-23 t)
      (message "* Running on Emacs 23")
      )
    ;; Emacs 24.x
    ((= emacs-major-version 24)
      (setq running-on-emacs-24 t)
      (when (= emacs-minor-version 4)
        (setq running-on-emacs-24-4 t)
        )
      (when (= emacs-minor-version 5)
        (setq running-on-emacs-24-5 t)
        )
      (message "* Running on Emacs 24")
      )
    ((= emacs-major-version 25)
      (setq running-on-emacs-25 t)
      (message "* Running on Emacs 25")
      )
    )
  (message "  0.2 Version Recognition... Done"))

;;
;;; OS RECOGNITION
(when section-environment-os-recognition (message "  0.3 OS Recognition...")
  (if (string-equal system-type "windows-nt")
    ;; OS - Microsoft Windows
    (progn
      (defvar running-on-ms-windows t)
      (defvar running-on-gnu-linux nil)
      (message "* Running on MS-Window")
      )
    ;; OS - GNU / Linux
    (progn
      (defvar running-on-ms-windows nil)
      (defvar running-on-gnu-linux t)
      (message "* Running on GNU/Linux")
      )
    )
  (message "  0.3 OS Recognition... Done"))

;;
;;; TERMINAL VS GRAPHICS
(when section-environment-terminal-vs-graphics (message "  0.4 Terminal VS Graphics...")
  ;; or (display-graphic-p) ? it works like this in MS Windows
  (if (window-system)
    (progn
      (defvar running-in-graphical t)
      (defvar running-in-terminal nil)
      (message "* Running in graphical")
      )
    (progn
      (defvar running-in-graphical nil)
      (defvar running-in-terminal t)
      (message "* Running in terminal")
      )
    )
  (message "  0.4 Terminal VS Graphics... Done"))

;;
;;; SET PATH
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-set-path (message "  0.5 Set Path...")
  ;; to integrate Cygwin and msys and others executables with emacs
  (setenv "PATH" (concat profile-path (getenv "PATH")))

  (setq exec-path profile-exec-path)
  (setenv "LANG" profile-lang)
  (setenv "GTAGSGLOBAL" profile-gnu-global)
  (message "  0.5 Set Path... Done"))

;;
;;; MS WINDOWS PERFORMANCE
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-ms-windows-performance (message "  0.6 MS Windows: improve performance...")
  ;; This sets garbage collection to hundred times of the default.  Supposedly
  ;; significantly speeds up startup time. (Seems to work for me, but my
  ;; computer is pretty modern. Disable if you are on anything less than 1
  ;; ghz).
  (setq gc-cons-threshold 50000000)
  ;;
  ;; try to improve slow performance on windows.
  (when (and section-environment-os-recognition running-on-ms-windows)
    (setenv "CYGWIN" "nodosfilewarning")
    (setq w32-get-true-file-attributes nil)
    )
  ;;;; do not wait fully redisplay before take in keyboard/mouse event
  ;;(setq redisplay-dont-pause nil)
  (message "  0.6 Windows: improve performance... Done"))

;;
;;; EXECUTABLE
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-executable (message "  0.7 Executable...")
  (custom-set-variables
    ;'(shell-file-name profile-shell-file-name)
    '(ediff-diff-program profile-ediff-diff-program)
    '(ediff-diff3-program profile-ediff-diff3-program)
    '(ediff-cmp-program profile-ediff-cmp-program)
    )
  (setq explicit-bash-args '("--login" "-i"))
  (message "  0.7 Executable... Done"))

;;
;;; ELPA
(when section-environment-elpa (message "  0.8 ELPA...")
  (when (and section-environment-version-recognition (not running-on-emacs-23))
    (when profile-environment-elpa-proxy-http
     ;; add to load path the profile directory
     (require 'url) ; need to have url-proxy-services defined
     ;; backquot instead of quote will allow use of comma to evaluate symbol after
     ;; example:
     ;;  (setq toto 12)
     ;;  (setq list '(("tata" . 42)))
     ;;  (add-to-list 'list `("titi" . ,toto))
     ;; result into a list (("titi" . 12) ("tata" . 42))
     (add-to-list 'url-proxy-services '("no_proxy" . "localhost") t)
     (add-to-list 'url-proxy-services `("http" . ,profile-environment-elpa-proxy-http) t)
     (add-to-list 'url-proxy-services `("https" . ,profile-environment-elpa-proxy-https) t))

    (when (try-require 'package "    ")
      ;; set package server
      (add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/") t)
      (add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/") t)
      ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
      (add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
      ;; set proxy
      ;; set path where put all packages
      (setq package-user-dir (concat (file-name-as-directory dotemacs-path) "plugins/elpa"))
      ;; init package
      (package-initialize)
      ;; refresh package list only when no package were found
      (when (not package-archive-contents)
        (package-refresh-contents))
      ;; install package from list defined in profile
      (mapc #'(lambda (package)
                (unless (package-installed-p package)
                  (package-install package)))
        profile-environment-elpa-package-list)
      )
    )
  (message "  0.8 ELPA... Done"))

;;
;;; HYPER (shortcut with "H-")
;; be careful with this it fully disable menu key when Emacs has focus
(when section-environment-hyper (message "  0.9 Hyper...")
  (when (and section-environment-os-recognition running-on-ms-windows)
    (setq
      w32-pass-apps-to-system nil
      w32-apps-modifier 'hyper) ;; Menu key
    )
  (message "  0.9 Hyper... Done"))

;;
;;; SUPER (shortcut with "s-")
;; be careful with this it fully disable windows key when Emacs has focus
(when section-environment-super (message "  0.10 Super...")
  (when (and section-environment-os-recognition running-on-ms-windows)
    ;; setting the PC keyboard's various keys to
    ;; Super or Hyper, for emacs running on Windows.
    (setq
      w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-lwindow-modifier 'super ;; Left Windows key
      w32-rwindow-modifier 'super) ;; Right Windows key
    )
  (message "  0.10 Super... Done"))

;;
;;; SERVER
;; start the emacs server to have only one emacs client
(when section-environment-server (message "  0.11 Server...")
  (when (and section-environment-terminal-vs-graphics running-in-graphical)
    (server-start)
    (message "* Running with server")
    )
  (message "  0.11 Server... Done"))


(provide 'environment)

;;; environment.el ends here
