;;; 02-mode-055-ps2pdf.el --- configuration of ps2pdf mode

;; Copyright (c) 2017-2020 Claude Tete
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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [[SUBHEADER.print buffer/region in pdf (the pdf background is unavoidably white so dark
;; theme don't render good)
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package ps2pdf
  :config
  (defalias 'prr 'ps2pdf-from-region)
  (defalias 'prb 'ps2pdf-from-buffer))


(provide '02-mode-055-ps2pdf)

;;; 02-mode-055-ps2pdf.el ends here
