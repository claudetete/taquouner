;;; 02-mode-030-undo-tree.el --- configuration of undo tree mode

;; Copyright (c) 2017-2020 Claude Tete
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
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.replace the undo built in function]
;; [SUBDEFAULT.t]
;; deprecated, replaced by undo-fu (without tree visual)


;;; Code:
(use-package undo-tree
  :bind
  ("C-_"   . undo-tree-undo)
  ("M-_"   . undo-tree-redo)
  ("C-M-_" . undo-tree-visualize)

  :bind (:map undo-tree-visualizer-mode-map
          ;; enter and 'q' key will quit undo tree
          ("RET" . (lambda ()
                     (interactive)
                     (undo-tree-visualizer-quit)
                     (when section-mode-cedet-ecb
                       (ecb-toggle-compile))))
          ("q" . (lambda ()
                   (interactive)
                   (undo-tree-visualizer-quit)
                   (when section-mode-cedet-ecb
                     (ecb-toggle-compile))))
          ;; page up/down will undo/redo 10 times
          ("<prior>" . (lambda ()
                         (interactive)
                         (cl-loop repeat 10 do
                           (undo-tree-visualize-undo))))
          ("<next>" . (lambda ()
                        (interactive)
                        (cl-loop repeat 10 do
                          (undo-tree-visualize-redo))))
          ;; inverse C-up/down
          ("<C-up>" . undo-tree-visualize-undo-to-x)
          ("<C-down>" . undo-tree-visualize-redo-to-x)
          )

  :init
  ;; enable globally in all mode
  (global-undo-tree-mode t)
  )


(provide '02-mode-030-undo-tree)

;;; 02-mode-030-undo-tree.el ends here
