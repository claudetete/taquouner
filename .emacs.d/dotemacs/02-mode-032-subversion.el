;;; 02-mode-032-subversion.el --- configuration of subversion mode

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
;; [SUBHEADER.support Subversion 1.7]
;; [SUBDEFAULT.nil]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:
(try-require 'vc-svn17 "    ")


(if tqnr-section-mode-cedet-ecb
  (progn
    ;; make 'q' quit buffer and close window
    ;; in diff mode
    (add-hook 'svn-status-diff-mode-hook
      (lambda ()
        (define-key svn-status-diff-mode-map      "q"     '(lambda ()
                                                             (interactive)
                                                             (delete-window)
                                                             (ecb-toggle-compile)))))
    ;; in log mode
    (add-hook 'svn-log-view-mode-hook
      (lambda ()
        (define-key svn-log-view-mode-map         "q"     '(lambda ()
                                                             (interactive)
                                                             (delete-window)
                                                             (ecb-toggle-compile)))))
    ) ; (progn
  (progn
    ;; make 'q' quit buffer and close window
    ;; in diff mode
    (add-hook 'svn-status-diff-mode-hook
      (lambda ()
        (define-key svn-status-diff-mode-map      "q"     'delete-window)))
    ;; in log mode
    (add-hook 'svn-log-view-mode-hook
      (lambda ()
        (define-key svn-log-view-mode-map         "q"     'delete-window)))
    ) ; (progn
  ) ; (if section-mode-cedet-ecb


(provide '02-mode-032-subversion)

;;; 02-mode-032-subversion.el ends here
