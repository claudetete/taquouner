;;; 02-mode-078-markdown.el --- configuration of markdown mode

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
;; [SUBHEADER.mode to edit Markdown-formatted text (by example wiki of github)]
;; [SUBDEFAULT.t]


;;; Code:
(when (try-require 'autoload-markdown-mode "    ")
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

  ;; [[VARCOMMENT.to enable markdown mode with github flavoured for all .md files and not
  ;; only for README.md
  ;; ]]
  ;; [VARIABLE.tqnr-section-mode-markdown-github t]
  (if tqnr-section-mode-markdown-github
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
    (progn
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
      (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))
    )
  )


(provide '02-mode-078-markdown)

;;; 02-mode-078-markdown.el ends here
