;;; environment.el --- a config file for environment settings

;; Copyright (c) 2006, 2007, 2008, 2009, 2010, 2011, 2012 Claude Tete
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
;; Version: 2.3
;; Created: October 2006
;; Last-Updated: June 2012

;;; Commentary:
;;
;; load by `emacs.el' (where all requirements are defined)
;; REQUIREMENT: var     `section-environment'
;;              var     `profile'
;;              var     `dotemacs-path'

;;; Change Log:
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
;;; OS RECOGNITION
(when section-environment-os-recognition (message "  0.2 OS Recognition...")
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
  (message "  0.2 OS Recognition... Done"))

;;
;;; TERMINAL VS GRAPHICS
(when section-environment-terminal-vs-graphics (message "  0.3 Terminal VS Graphics...")
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
  (message "  0.3 Terminal VS Graphics... Done"))

;;
;;; SET PATH
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-set-path (message "  0.4 Set Path...")
  ;; to integrate Cygwin and msys and others executables with emacs
  (setenv "PATH" (concat profile-path (getenv "PATH")))

  (setq exec-path profile-exec-path)
  (setenv "LANG" profile-lang)
  (setenv "GTAGSGLOBAL" profile-gnu-global)
  (message "  0.4 Set Path... Done"))

;;
;;; MS WINDOWS PERFORMANCE
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-ms-windows-performance (message "  0.5 MS Windows: improve performance...")
  ;; This sets garbage collection to hundred times of the default.  Supposedly
  ;; significantly speeds up startup time. (Seems to work for me, but my
  ;; computer is pretty modern. Disable if you are on anything less than 1
  ;; ghz).
  (setq gc-cons-threshold 50000000)
  ;;
  ;; try to improve slow performance on windows.
  (if running-on-ms-windows
    (setq w32-get-true-file-attributes nil))
  (message "  0.5 Windows: improve performance... Done"))

;;
;;; EXECUTABLE
;; REQUIREMENT: var     `section-environment-os-recognition'
(when section-environment-executable (message "  0.6 Executable...")
  (custom-set-variables
    '(shell-file-name profile-shell-file-name)
    )
  (setq explicit-bash-args '("--login" "-i"))
  (message "  0.6 Executable... Done"))

;;
;;; ELPA
(when section-environment-elpa (message "  0.7 ELPA...")
  ;; add to load path the profile directory
  (add-to-list 'load-path (concat dotemacs-path "/plugins/elpa"))
  (setq load-path (cons (expand-file-name (concat dotemacs-path "/plugins/elpa")) load-path))

  ;; This was installed by package-install.el.  This provides support for the
  ;; package system and interfacing with ELPA, the package archive.  Move this
  ;; code earlier if you want to reference packages in your .emacs.
  (when (try-require 'package "    ") (package-initialize))

  ;; set path where put all packages
  (setq package-user-dir (concat dotemacs-path "/plugins/elpa"))

  ;;;; set package server
  ;;; only with Emacs 24 ?
  (setq package-archives
    '(
       ("ELPA"      . "http://tromey.com/elpa/")
       ("gnu"       . "http://elpa.gnu.org/packages/")
       ("marmalade" . "http://marmalade-repo.org/packages/")
       ("melpa"     . "http://melpa.milkbox.net/packages/")
       )
    )
  (message "  0.7 ELPA... Done"))

;;
;;; HYPER
;; be careful with this it fully disable menu key when Emacs has focus
(when section-environment-hyper (message "  0.8 Hyper...")
  (when running-on-ms-windows
    (setq
      w32-pass-apps-to-system nil
      w32-apps-modifier 'hyper) ;; Menu key
    )
  (message "  0.8 Hyper... Done"))

;;
;;; SUPER
;; be careful with this it fully disable windows key when Emacs has focus
(when section-environment-super (message "  0.9 Super...")
  (when running-on-ms-windows
    ;; setting the PC keyboard's various keys to
    ;; Super or Hyper, for emacs running on Windows.
    (setq
      w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-lwindow-modifier 'super ;; Left Windows key
      w32-rwindow-modifier 'super) ;; Right Windows key
    )
  (message "  0.9 Super... Done"))


(provide 'environment)

;;; environment.el ends here
