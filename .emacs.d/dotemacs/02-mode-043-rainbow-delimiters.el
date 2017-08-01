;;; 02-mode-043-rainbow-delimiters.el --- configuration of rainbow delimiters mode

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
;; [SUBHEADER.highlight nested parentheses, brackets in different color depending of depth]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:

(when (try-require 'autoload-rainbow-delimiters "    ")
  ;; set dark background
  (setq-default frame-background-mode 'dark)
  (when (and tqnr-section-environment-version-recognition (not tqnr-running-on-emacs-23))
    ;; enable this mode in programming mode
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  )


(provide '02-mode-043-rainbow-delimiters)

;;; 02-mode-043-rainbow-delimiters.el ends here
