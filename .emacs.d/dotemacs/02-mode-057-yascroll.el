;;; 02-mode-057-yascroll.el --- configuration of yascroll mode

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
;; [[SUBHEADER.add a small visual scroll-bar (can not be used with mouse click)
;; see https://github.com/m2ym/yascroll-el for screenshot
;; ]]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'yascroll "    ")
  (global-yascroll-bar-mode t)

  ;; [VARCOMMENT.time before hide scroll-bar (nil to always show)]
  ;; [VARIABLE.tqnr-profile-yascroll-delay-to-hide nil]
  (setq yascroll:delay-to-hide tqnr-profile-yascroll-delay-to-hide))


(provide '02-mode-057-yascroll)

;;; 02-mode-057-yascroll.el ends here
