;;; 02-mode-022-vc-clearcase.el --- configuration of vc clearcase mode

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
;; [SUBHEADER.vc ClearCase mode (not used)]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:

;;
;;; VC CLEARCASE ; do not work
(add-to-list 'load-path  (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/vc-clearcase-3.6"))
(try-require 'vc-clearcase-auto "    ")
(custom-set-variables
  '(clearcase-checkout-comment-type (quote normal))
  '(clearcase-use-external-diff t)
  ;; [VARCOMMENT.path to version tree executable]
  ;; [VARIABLE.tqnr-profile-clearcase-vtree "clearvtree.exe"]
  '(clearcase-vtree-program tqnr-profile-clearcase-vtree)
  ;; [VARCOMMENT.path to cleartool executable]
  ;; [VARIABLE.tqnr-profile-cleartool "cleartool.exe"]
  '(cleartool-program tqnr-profile-cleartool)
  )


(provide '02-mode-022-vc-clearcase)

;;; 02-mode-022-vc-clearcase.el ends here
