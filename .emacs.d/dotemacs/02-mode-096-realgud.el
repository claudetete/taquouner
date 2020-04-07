;;; 02-mode-096-realgud.el --- configuration of realgud mode -*- lexical-binding: t -*-

;; Copyright (c) 2019-2020 Claude Tete
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
;; Created: May 2019
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.realgud mode]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package realgud
  :pin melpa

  :config
  ;; to work with pdb
  (setq realgud:pdb-command-name "python -m pdb")
  ;; to work with ipdb
  (setq realgud:ipdb-command-name "python -m ipdb")
  ) ;; (use-package realgud


(provide '02-mode-096-realgud)

;;; 02-mode-096-realgud.el ends here
