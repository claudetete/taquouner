;;; 02-mode-046-fold-dwim.el --- configuration of fold dwim mode

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
;; [SUBHEADER.show hide code source block]
;; [SUBDEFAULT.t]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(try-require 'autoload-fold-dwim "    ")


;; only in C mode
(add-hook 'c-mode-hook
  (lambda ()
    ;; show/hide block
    (local-set-key      (kbd "<H-left>")        'fold-dwim-toggle)
    (local-set-key      (kbd "<H-right>")       'fold-dwim-toggle)
    ;; hide all
    (local-set-key      (kbd "<H-up>")          'fold-dwim-hide-all)
    ;; show all
    (local-set-key      (kbd "<H-down>")        'fold-dwim-show-all)
    )
  )

;; only when rtrt is used
(when tqnr-section-mode-rtrt-script
  (add-hook 'rtrt-script-mode-hook
    (lambda ()
      ;; hide all
      (local-set-key    (kbd "<H-prior>")       'fold-dwim-hide-all)
      ;; show all
      (local-set-key    (kbd "<H-next>")        'fold-dwim-show-all)
      )
    )
  )


(provide '02-mode-046-fold-dwim)

;;; 02-mode-046-fold-dwim.el ends here
