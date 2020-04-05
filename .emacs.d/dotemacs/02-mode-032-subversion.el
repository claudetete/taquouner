;;; 02-mode-032-subversion.el --- configuration of subversion mode

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
;; [SUBHEADER.support Subversion 1.7]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package vc-svn17
  :load-path (lambda () (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/vc-svn17.el"))

(if tqnr-section-mode-cedet-ecb
  (use-package vc-svn17
    :bind (:map svn-status-diff-mode-map
            ;; make 'q' quit buffer and close window
            ;; in diff mode
            ("q" . (lambda ()
                     (interactive)
                     (delete-window)
                     (ecb-toggle-compile)))
            :map svn-log-view-mode-map
            ("q" . (lambda ()
                     (interactive)
                     (delete-window)
                     (ecb-toggle-compile)))))
  (use-package vc-svn17
    :bind (:map svn-status-diff-mode-map
            ;; make 'q' quit buffer and close window
            ;; in diff mode
            ("q" . delete-window)
            :map svn-log-view-mode-map
            ;; in log mode
            ("q" . delete-window)))
  ) ;; (if tqnr-section-mode-cedet-ecb


(provide '02-mode-032-subversion)

;;; 02-mode-032-subversion.el ends here
