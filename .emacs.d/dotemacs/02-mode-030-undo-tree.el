;;; 02-mode-030-undo-tree.el --- configuration of undo tree mode

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
;; [SUBHEADER.replace the undo built in function]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(when (try-require 'autoload-undo-tree "    ")
  ;; enable globally in all mode
  (global-undo-tree-mode t)
  )


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (global-set-key     (kbd "C-_")             'undo-tree-undo)
    (global-set-key     (kbd "M-_")             'undo-tree-redo)
    (global-set-key     (kbd "C-M-_")           'undo-tree-visualize)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(add-hook 'undo-tree-visualizer-mode-hook
  (lambda ()
    ;; enter and 'q' key will quit undo tree
    (local-set-key      (kbd "RET")             (lambda ()
                                                  (interactive)
                                                  (undo-tree-visualizer-quit)
                                                  (when section-mode-cedet-ecb
                                                    (ecb-toggle-compile))))
    (local-set-key      (kbd "q")               (lambda ()
                                                  (interactive)
                                                  (undo-tree-visualizer-quit)
                                                  (when section-mode-cedet-ecb
                                                    (ecb-toggle-compile))))
    ;; page up/down will undo/redo 10 times
    (local-set-key      (kbd "<prior>")         (lambda ()
                                                  (interactive)
                                                  (cl-loop repeat 10 do
                                                    (undo-tree-visualize-undo))))
    (local-set-key      (kbd "<next>")          (lambda ()
                                                  (interactive)
                                                  (cl-loop repeat 10 do
                                                    (undo-tree-visualize-redo))))
    ;; inverse C-up/down
    (local-set-key      (kbd "<C-up>")          'undo-tree-visualize-undo-to-x)
    (local-set-key      (kbd "<C-down>")        'undo-tree-visualize-redo-to-x)
    )
  )


(provide '02-mode-030-undo-tree)

;;; 02-mode-030-undo-tree.el ends here
