;;; custom.el --- a config file to put all custom option from `customize'

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

;; Keywords: config, customize
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.0
;; Created: October 2006
;; Last-Updated: July 2011

;;; Commentary:
;;
;; load by emacs.el (where all requirements are defined)
;; REQUIREMENT: var     `section-filecustomize'
;;
;; be careful it is the last loaded file, so everything emacs put in will
;; overwrite your other settings

;;; Change Log:
;; 2011-03-10 (1.0)
;;    split .emacs file
;; 2010-06-11 (0.1)
;;    creation from scratch


;;; Code:
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(rtrt-script-indent 4)
 )

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)
