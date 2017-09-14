;;; 02-mode-021-rtrt-script.el --- configuration of rtrt script mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: September 2017

;;; Commentary:
;;
;; [SUBHEADER.rtrt script mode (syntax coloration)]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-09-14 (0.2)
;;    remove outline setting, it is redundant with specific outline mode setting
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'rtrt-ptu "    ")
  )


(provide '02-mode-021-rtrt-script)

;;; 02-mode-021-rtrt-script.el ends here
