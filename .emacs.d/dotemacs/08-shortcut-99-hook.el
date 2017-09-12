;;; 08-shortcut-99-hook.el --- a config file for shortcut hook

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
;; Version: 1.1
;; Created: October 2006
;; Last-Updated: July 2017

;;; Commentary:
;;
;; [SUBHEADER.run hook for all shortcuts from the whole configuration]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-21 (1.1)
;;    use hook to define binding/shortcut
;; 2012-06-05 (1.0)
;;    split from emacs.el
;; 2006-10-13 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;; run all hook about shortcuts
(run-hooks 'tqnr-after-init-shortcut-hook)


(provide '08-shortcut-99-hook)

;;; 08-shortcut-99-hook.el ends here
