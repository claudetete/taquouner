;;; 02-mode-028-fill-column-indicator.el --- configuration of fill column indicator mode -*- lexical-binding: t -*-

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.show a vertical line at fill-column column or customize it]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package fill-column-indicator
  :config
  ;; [VARCOMMENT.pixel width of vertical line default 1 (nil)]
  ;; [VARIABLE.tqnr-profile-fill-column-indicator-vertical-line-width nil]
  (when tqnr-profile-fill-column-indicator-vertical-line-width
    (setq fci-rule-width tqnr-profile-fill-column-indicator-vertical-line-width))
  ;; [VARCOMMENT.color of vertical line in color format or nil (set comment theme face)]
  ;; [VARIABLE.tqnr-profile-fill-column-indicator-vertical-line-color nil]
  (if tqnr-profile-fill-column-indicator-vertical-line-color
    (setq fci-rule-color tqnr-profile-fill-column-indicator-vertical-line-color)
    ;; use shortcut hook to be sure it is eval at end of configuration after setting theme
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (setq fci-rule-color (face-foreground 'font-lock-comment-face))
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    )

  ;; [VARCOMMENT.Use a fixed column for vertical line to not use fill-column value otherwise nil]
  ;; [VARIABLE.tqnr-profile-fill-column-indicator-vertical-line-position nil]
  (when tqnr-profile-fill-column-indicator-vertical-line-position
    (setq fci-rule-column tqnr-profile-fill-column-indicator-vertical-line-position))
  ) ;; (use-package fill-column-indicator

;; [VARCOMMENT.enable vertical line in C mode]
;; [VARIABLE.tqnr-profile-fill-column-indicator-mode-c nil]
(when tqnr-profile-fill-column-indicator-mode-c
  (use-package fill-column-indicator
    :hook (c-mode-hook . fci-mode)))

;; [VARCOMMENT.enable vertical line in C++ mode]
;; [VARIABLE.tqnr-profile-fill-column-indicator-mode-c++ nil]
(when tqnr-profile-fill-column-indicator-mode-c++
  (use-package fill-column-indicator
    :hook (c++-mode-hook . fci-mode)))

;; [VARCOMMENT.enable vertical line in ADA mode]
;; [VARIABLE.tqnr-profile-fill-column-indicator-mode-ada nil]
(when tqnr-profile-fill-column-indicator-mode-ada
  (use-package fill-column-indicator
    :hook (ada-mode-hook . fci-mode)))

;; [VARCOMMENT.enable vertical line in all mode]
;; [VARIABLE.tqnr-profile-fill-column-indicator-mode-all nil]
(when tqnr-profile-fill-column-indicator-mode-all
  (use-package fill-column-indicator
    :hook (after-change-major-mode-hook . fci-mode)))

;; do not enable truncate-line when not wanted
(when tqnr-section-annoyances-truncate-line
  (use-package fill-column-indicator
    :custom
    (fci-handle-truncate-lines nil)))


(provide '02-mode-028-fill-column-indicator)

;;; 02-mode-028-fill-column-indicator.el ends here
