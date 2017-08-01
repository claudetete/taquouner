;;; 00-environment.el --- a config file for environment settings

;; Copyright (c) 2006-2017 Claude Tete
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
;; Version: 3.2
;; Created: October 2006
;; Last-Updated: May 2017

;;; Commentary:
;; [HEADER.Environment check and configuration]

;;; Change Log:
;; 2017-05-26 (3.1)
;;    remove useless settings about super/hyper for MS Windows
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
;; [SUBCOMMENT.VERSION RECOGNITION: detect Emacs version]
;; [SUBSECTION.tqnr-section-environment-version-recognition t]
(when tqnr-section-environment-version-recognition (message "    Version Recognition...")
  (defvar tqnr-running-on-emacs-23 nil)
  (defvar tqnr-running-on-emacs-24 nil)
  (defvar tqnr-running-on-emacs-24-4 nil)
  (defvar tqnr-running-on-emacs-24-5 nil)
  (defvar tqnr-running-on-emacs-25 nil)
  (cond
    ;; Emacs 23.x
    ((= emacs-major-version 23)
      (setq tqnr-running-on-emacs-23 t)
      (message "* Running on Emacs 23")
      )
    ;; Emacs 24.x
    ((= emacs-major-version 24)
      (setq tqnr-running-on-emacs-24 t)
      (when (= emacs-minor-version 4)
        (setq tqnr-running-on-emacs-24-4 t)
        )
      (when (= emacs-minor-version 5)
        (setq tqnr-running-on-emacs-24-5 t)
        )
      (message "* Running on Emacs 24")
      )
    ((= emacs-major-version 25)
      (setq tqnr-running-on-emacs-25 t)
      (message "* Running on Emacs 25")
      )
    )
  (message "    Version Recognition... Done"))

;;
;; [SUBCOMMENT.OS RECOGNITION: detect OS type (MS Windows vs GNU Linux)]
;; [SUBSECTION.tqnr-section-environment-os-recognition t]
(when tqnr-section-environment-os-recognition (message "    OS Recognition...")
  (if (string-equal system-type "windows-nt")
    ;; OS - Microsoft Windows
    (progn
      (defvar tqnr-running-on-ms-windows t)
      (defvar tqnr-running-on-gnu-linux nil)
      (message "* Running on MS-Window")
      )
    ;; OS - GNU / Linux
    (progn
      (defvar tqnr-running-on-ms-windows nil)
      (defvar tqnr-running-on-gnu-linux t)
      (message "* Running on GNU/Linux")
      )
    )
  (message "    OS Recognition... Done"))

;;
;; [SUBCOMMENT.TERMINAL VS GRAPHICS: detect Emacs terminal vs graphics]
;; [SUBSECTION.tqnr-section-environment-terminal-vs-graphics t]
(when tqnr-section-environment-terminal-vs-graphics (message "    Terminal VS Graphics...")
  ;; or (display-graphic-p) ? it works like this in MS Windows
  (if (window-system)
    (progn
      (defvar tqnr-running-in-graphical t)
      (defvar tqnr-running-in-terminal nil)
      (message "* Running in graphical")
      )
    (progn
      (defvar tqnr-running-in-graphical nil)
      (defvar tqnr-running-in-terminal t)
      (message "* Running in terminal")
      )
    )
  (message "    Terminal VS Graphics... Done"))

;;
;;; SET PATH
;; [SUBCOMMENT.SET PATH: Set environment variable PATH]
;; [SUBSECTION.tqnr-section-environment-set-path t]
(when tqnr-section-environment-set-path (message "    Set Path...")
  ;; [VARCOMMENT.PATH environment variable concat with current PATH]
  ;; [VARIABLE.tqnr-profile-path (concat "" "")]
  (setenv "PATH" (concat tqnr-profile-path (getenv "PATH")))

  ;; [COMMENT.]
  ;; [VARCOMMENT.emacs can also search in this path exec for external tool]
  ;; [VARIABLE.tqnr-profile-exec-path '()]
  (setq exec-path tqnr-profile-exec-path)
  ;; [COMMENT.]
  ;; [VARCOMMENT.LOCALE: languages settings about subversion and dired]
  ;; [VARIABLE.tqnr-profile-lang "en_US"]
  (setenv "LANG" tqnr-profile-lang)
  (setenv "GTAGSGLOBAL" tqnr-profile-gnu-global)
  (message "    Set Path... Done"))

;;
;; [SUBCOMMENT.MS WINDOWS PERFORMANCE: MS Windows specific configuration about performance]
;; [SUBSECTION.tqnr-section-environment-ms-windows-performance t]
(when tqnr-section-environment-ms-windows-performance (message "    MS Windows: improve performance...")
  ;; This sets garbage collection to hundred times of the default.  Supposedly
  ;; significantly speeds up startup time. (Seems to work for me, but my
  ;; computer is pretty modern. Disable if you are on anything less than 1
  ;; ghz).
  (setq gc-cons-threshold (* 511 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t)
  ;;
  ;; try to improve slow performance on windows.
  (when (and tqnr-section-environment-os-recognition tqnr-running-on-ms-windows)
    (setenv "CYGWIN" "nodosfilewarning")
    (setq w32-get-true-file-attributes nil)
    )
  (if (>= emacs-major-version 25)
    ;;
    (setq w32-pipe-read-delay 0)
    ;; Set the buffer size to 64K on windows (from the original 4K)
    (setq w32-pipe-buffer-size (* 64 1024)))
  ;;;; do not wait fully redisplay before take in keyboard/mouse event
  ;;(setq redisplay-dont-pause nil)
  (message "    Windows: improve performance... Done"))

;;
;; [SUBCOMMENT.EXECUTABLE: Set path of some exe]
;; [SUBSECTION.tqnr-section-environment-executable t]
(when tqnr-section-environment-executable (message "    Executable...")
  (custom-set-variables
    ;; [VARCOMMENT.diff program]
    ;; [VARIABLE.tqnr-profile-ediff-diff-program "diff"]
    '(ediff-diff-program tqnr-profile-ediff-diff-program)
    ;; [VARIABLE.tqnr-profile-ediff-diff3-program "diff3"]
    '(ediff-diff3-program tqnr-profile-ediff-diff3-program)
    ;; [VARIABLE.tqnr-profile-ediff-cmp-program "cmp"]
    '(ediff-cmp-program tqnr-profile-ediff-cmp-program)
    )
  (setq explicit-bash-args '("--login" "-i"))
  (message "    Executable... Done"))

;;
;; [SUBCOMMENT.HYPER: Enable Hyper modifier key (Menu key, shortcut with "H-") on MS Windows]
;; [SUBSECTION.tqnr-section-environment-hyper t]
;; be careful with this it fully disable menu key when Emacs has focus
(when tqnr-section-environment-hyper (message "    Hyper...")
  (when (and tqnr-section-environment-os-recognition tqnr-running-on-ms-windows)
    (setq
      w32-pass-apps-to-system nil
      w32-apps-modifier 'hyper) ;; Menu key
    )
  (message "    Hyper... Done"))

;;
;; [SUBCOMMENT.SUPER: Enable Super modifier key (Windows key, shortcut with "s-") on MS Windows]
;; [SUBSECTION.tqnr-section-environment-super t]
;; be careful with this it fully disable windows key when Emacs has focus
(when tqnr-section-environment-super (message "    Super...")
  (when (and tqnr-section-environment-os-recognition tqnr-running-on-ms-windows)
    ;; setting the PC keyboard's various keys to
    ;; Super or Hyper, for emacs running on Windows.
    ;; Left Windows key
    ;(setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super)
    ;; Right Windows key
    ;(setq w32-pass-rwindow-to-system nil)
    (setq w32-rwindow-modifier 'super)
    )
  (message "    Super... Done"))

;;
;; [SUBCOMMENT.SERVER: start the emacs server to have only one emacs client instance]
;; [SUBSECTION.tqnr-section-environment-server t]
(when tqnr-section-environment-server (message "    Server...")
  (when (and tqnr-section-environment-terminal-vs-graphics tqnr-running-in-graphical)
    (server-start)
    (message "* Running with server")
    )
  (message "    Server... Done"))


(provide '00-environment)

;;; 00-environment.el ends here
