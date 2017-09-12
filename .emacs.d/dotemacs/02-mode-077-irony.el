;;; 02-mode-077-irony.el --- configuration of irony mode

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
;; [SUBHEADER.improving the editing experience for the C, C++ and Objective-C using clang]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-07-25 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
;; irony work fine on linux but only with emacs >= 24.4 on ms windows
(if (or
      tqnr-running-on-gnu-linux
      (and tqnr-running-on-ms-windows
        (or
          tqnr-running-on-emacs-24-4
          tqnr-running-on-emacs-24-5
          tqnr-running-on-emacs-25)))
  (progn
    ;; install from list of packages
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
    )
  (message "* Irony mode is not compatible with your environment")
  )

(when tqnr-running-on-ms-windows
  ;; Windows performance tweaks
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
    ;;
  ;; this options is only since emacs 25
  (when tqnr-running-on-emacs-25
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
    )
  )

;; Irony for MS Windows (extract from wiki of github + small add)
;; IRONY-SERVER build (a compiled version is available in this repository
;; plugins/irony-mode.zip)
;;
;; Prerequisites: Msys2, and MinGW packages.
;;
;; clang and cmake were installed through Msys2/MinGW's pacman and are at
;; standard locations. These are the steps taken to get irony-mode to work.
;; (you can use mingw64 package, but used gcc and clang must all be from
;; mingw64 or mingw32 or msys)
;;
;; git clone https://github.com/Sarcasm/irony-mode.git ~/irony-mode
;;
;; Add in your .bashrc (or shell init), path location of binaries:
;; # MSYS
;; export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
;; # MINGW x86_64 (to use mingw64 installed packages)
;; export PATH="/mingw64/bin:$PATH"
;;
;; Open a mingw{32, 64} shell. cd into the irony-mode server sub-directory,
;; $ cd irony-mode/server
;; $ mkdir build
;;
;; The find module that comes with this project should be able to find
;; libclang, which is called clang.dll in MinGW Packages.
;;
;; To Build (next step only if no error):
;; $ cd build
;; $ cmake -G "MSYS Makefiles" ..
;; or when using mingw64 package
;; $ cmake -DLIBCLANG_LIBRARY=/mingw64/bin/clang.dll -G "MSYS Makefiles" ..
;; $ make -j 4


(provide '02-mode-077-irony)

;;; 02-mode-077-irony.el ends here
