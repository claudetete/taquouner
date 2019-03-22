;;; 02-mode-007-batch.el --- configuration of batch mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.mode for .bat script in MS Windows]
;; [SUBDEFAULT.t]


;;; Code:

(autoload 'batch-mode "batch-mode" "Load batch-mode")
(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))


(provide '02-mode-007-batch)

;;; 02-mode-007-batch.el ends here
