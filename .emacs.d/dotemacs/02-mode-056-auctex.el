;;; 02-mode-056-auctex.el --- a config file for auctex mode -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 Claude Tete
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
;; Version: 0.3
;; Created: January 2013
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.latex mode]
;; [SUBDEFAULT.nil]
;;
;; Requirements:
;;  For MS Windows:
;;  - libpng/zlib  (http://www.gtk.org/download/win32.php)
;;  - libtiff (http://gnuwin32.sourceforge.net/packages/tiff.htm)
;;  - giflib (http://gnuwin32.sourceforge.net/packages/giflib.htm)
;;  - libjpeg (http://gnuwin32.sourceforge.net/packages/jpeg.htm)
;;  - xpmlib (http://gnuwin32.sourceforge.net/packages/xpm.htm)
;;  - unzip all in emacs-2x.x/bin/
;;  - LaTeX engine (like MiKTex) (/bin folder must be in PATH environment
;;  - variable)
;;  - GhostScript (/bin folder must be in PATH environment variable)


;;; Code:
;; FIXME need to be updated for emacs > 24
;; load auctex
(load "auctex.el" nil t t)
;; load preview latex to change formula to picture
(load "preview-latex.el" nil t t)
;; mode reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; to have information about tex file in dvi file (for inverse search)
;; in YAP add as external editor:
;; C:\..\emacs-24.2\bin\emacsclientw.exe --alternate-editor="C:\..\emacs-24.2\bin\runemacs.exe" +%l "%f"
(setq LaTeX-command "latex --src-specials -synctex=1")
;;
(setq TeX-source-specials-mode t)
(setq TeX-source-specials-tex-flags "--src-specials")
;;
;; when on MS Windows MiKTeX is default engine
(when (and tqnr-section-environment-os-recognition tqnr-running-on-ms-windows)
  (try-require 'tex-mik "    "))

;; generate pdf by default
(setq TeX-PDF-mode t)

(setq-default TeX-master nil)
;; auto revert mode when in docview mode
;; auto-update the pdf file opened in emacs
(add-hook 'doc-view-mode-hook 'auto-revert-mode)


(add-hook 'LaTeX-mode-hook '
  (lambda ()
    (local-set-key      (kbd "<f10>")   'auctex-save-and-compile)
    ))


(provide '02-mode-056-auctex)

;;; 02-mode-056-auctex.el ends here
