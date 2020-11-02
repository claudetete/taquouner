;;; 00-environment.el --- a config file for environment settings -*- lexical-binding: t -*-

;; Copyright (c) 2006-2020 Claude Tete
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
;; Version: 3.5
;; Created: October 2006
;; Last-Updated: May 2020

;;; Commentary:
;; [HEADER.Environment check and configuration]
;; [DEFAULT.t]


;;; Code:

;;
;; [SUBCOMMENT.GARBAGE COLLECTION: speed up start of emacs]
;; [SUBSECTION.tqnr-section-environment-garbage-collection t]
(when tqnr-section-environment-garbage-collection
  ;; garbage collection to hundred times of the default. Supposedly
  ;; significantly speeds up startup time. see http://jonnay.github.io/emagicians-starter-kit/Emagician-Base.html#orgheadline3
  (defun garbage-collection-set-max ()
    ;; it means garbage collector will start only after eating 512MB of RAM
    (setq gc-cons-threshold (* 512 1024 1024)))

  (defun garbage-collection-set-default ()
    ;; return to default value, currently set to 800KB of RAM
    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))

  (defun garbage-collection-set-10MB ()
    ;; set to reasonable value 10MB of RAM
    (setq gc-cons-threshold (* 10 1024 1024)))

  ;; [VARCOMMENT.MINIBUFFER: increase temporarily garbage collection when execute something]
  ;; [VARIABLE.tqnr-section-environment-garbage-collection-minibuffer t]
  (when tqnr-section-environment-garbage-collection-minibuffer
    ;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
    ;; increase threshold when opening a minibuffer to execute something
    (add-hook 'minibuffer-setup-hook #'garbage-collection-set-10MB)
    ;; restore to default when exiting a minibuffer
    (add-hook 'minibuffer-exit-hook #'garbage-collection-set-default))

  ;; [VARCOMMENT.START: speed up a little the start of emacs]
  ;; [VARIABLE.tqnr-environment-garbage-collection-start nil]
  (when tqnr-environment-garbage-collection-start
    ;; set max of threshold of garbage collection to avoid running it during start-up
    (garbage-collection-set-max)
    ;; reset threshold after start-up
    (add-hook 'emacs-startup-hook #'garbage-collection-set-default))
  ) ;; (when tqnr-section-environment-garbage-collection

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
  ;; [[VARIABLE.tqnr-profile-path
  ;;   (list
  ;;     ;; before MSYS2 to make sure to use this version of tools
  ;;     ;; (do not put path which contains gcc otherwise it will be used instead of MSYS2 one)
  ;;     "C:/WinPython27/python-2.7.10"
  ;;     "C:/perl/perl/bin"
  ;;     "C:/Git/bin"
  ;;     ;; emacs useful binary
  ;;     (file-name-as-directory tqnr-dotemacs-path) "plugins/pt_windows_amd64"
  ;;     (file-name-as-directory tqnr-dotemacs-path) "plugins/gnu_global_656wb/bin"
  ;;     (file-name-as-directory tqnr-dotemacs-path) "plugins/cflow-mingw-master"
  ;;     (file-name-as-directory tqnr-dotemacs-path) "plugins/irony-mode/server/build/bin"
  ;;     ;; MSYS2
  ;;     "C:/MSYS2/usr/bin"
  ;;     "C:/MSYS2/mingw64/bin"
  ;;     ;; Python utils
  ;;     "C:/WinPython27/python-2.7.10/Scripts"
  ;;     ;; Perl utils
  ;;     "C:/perl/perl/site/bin"
  ;;     "C:/perl/c/bin"
  ;;     ;; Cygwin (useful ?)
  ;;     "C:/cygwin/bin"
  ;;     "C:/cygwin/usr/bin"
  ;;     "C:/cygwin/usr/local/bin"
  ;;     ;; GhostScript (useful for auctex)
  ;;     "C:/Program Files (x86)/gs/gs9.06/bin"
  ;;     ;; LaTeX engine
  ;;     "C:/MikTex/miktex/bin"
  ;;     ;; MS Windows usual path
  ;;     "C:/WINDOWS"
  ;;     "C:/WINDOWS/System32"
  ;;     ;; Aspell
  ;;     "C:/Program Files (x86)/Aspell/bin/"
  ;;     ;; Java engine (useful ?)
  ;;     "C:/ProgramData/Oracle/Java/javapath"
  ;;     ;; GraphViz
  ;;     "C:/GraphViz/bin"
  ;;     )
  ;; ]]
  ;; need to use (list ) instead of '() to be able to use variable in it see https://stackoverflow.com/questions/24188100/using-mapconcat-to-concatenate-a-list-containing-a-variable
  (setenv "PATH" (concat (mapconcat 'identity tqnr-profile-path ";") ";" (getenv "PATH")))
  ;; emacs can also search in this path exec for external tool
  (defvar exec-path-init exec-path)
  (setq exec-path (append tqnr-profile-path exec-path))
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
  ;;
  ;; try to improve slow performance on windows.
  (when (and tqnr-section-environment-os-recognition tqnr-running-on-ms-windows)
    (setenv "CYGWIN" "nodosfilewarning")
    (setq w32-get-true-file-attributes nil)
    (setq inhibit-compacting-font-caches t)
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
  ;; [VARCOMMENT.shell program]
  ;; [VARIABLE.tqnr-profile-shell-program "bash"]
  (setq shell-file-name tqnr-profile-shell-program)
  ;; [VARCOMMENT.shell program arguments]
  ;; [VARIABLE.tqnr-profile-shell-arguments "-lc"]
  (setq shell-command-switch tqnr-profile-shell-arguments)

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
;; [SUBSECTION.tqnr-section-environment-super nil]
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
;; [SUBSECTION.tqnr-section-environment-server nil]
(when tqnr-section-environment-server (message "    Server...")
  (when (and tqnr-section-environment-terminal-vs-graphics tqnr-running-in-graphical)
    (server-start)
    (message "* Running with server")
    )
  (message "    Server... Done"))


(provide '00-environment)

;;; 00-environment.el ends here
