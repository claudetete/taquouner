;;; 02-mode-001-helm.el --- configuration of helm mode -*- lexical-binding: t -*-

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
;; Version: 0.5
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.(fork ANYTHING) choose anything with the same nice interface]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package helm
  :bind (:map helm-map
          ;; keep old behavior about change directory instead of lef/right character
          ("<left>" . helm-previous-source)
          ("<right>" . helm-next-source)
          ;; quit helm with ESCAPE
          ("ESC"      . helm-keyboard-quit)
          ;; quit helm with f2
          ("<f2>"     . helm-keyboard-quit)
          ;; go to beginning of buffer
          ("M-<home>" . helm-beginning-of-buffer)
          ("C-<home>" . helm-beginning-of-buffer)
          ;; go to end of buffer
          ("M-<end>"  . helm-end-of-buffer)
          ("C-<end>"  . helm-end-of-buffer)
          ;; show previous element in history
          ("M-<up>"   . previous-history-element)
          ;; show next element in history
          ("M-<down>" . next-history-element))

  :custom
  ;; keep old behavior about change directory instead of lef/right character:
  ;; - for helm-find-files
  (helm-ff-lynx-style-map t)
  ;; - for helm-imenu
  (helm-imenu-lynx-style-map t)
  ;; - for semantic
  (helm-semantic-lynx-style-map t)
  ;; - for helm-occur
  (helm-occur-use-ioccur-style-keys t)
  ;; - for helm-grep
  (helm-grep-use-ioccur-style-keys t)

  :config
  ;; (custom-set-variables
  ;;   '(helm-follow-mode-persistent t))

  (setq helm-candidate-separator
    "--separator------------------------------")
  (when tqnr-section-mode-helm-buffers-list
    ;; to avoid error with helm-buffers-list
    (setq ido-use-virtual-buffers nil)
    ;; increase number of characters of buffer name before truncate
    (setq helm-buffer-max-length 80)
    )
  ) ;; (use-package helm


(when tqnr-section-mode-cedet-ecb
  (use-package helm
    :hook
    ;; quit helm when hide compile window
    (ecb-toggle-compile-hide-hook . helm-keyboard-quit)))


(when (and tqnr-section-mode-helm-bookmark tqnr-section-mode-helm-find-files)
  (use-package helm
    :custom
    (helm-type-bookmark-actions
      (quote
        ( ("Jump to bookmark"                 . helm-bookmark-jump)
          ("Jump to bookmark with find-files" . helm-bookmark-find-files-jump)
          ("Jump to BM other window"          . helm-bookmark-jump-other-window)
          ("Bookmark edit annotation"         . bookmark-edit-annotation)
          ("Bookmark show annotation"         . bookmark-show-annotation)
          ("Delete bookmark(s)"               . helm-delete-marked-bookmarks)
          ("Edit Bookmark"                    . helm-bookmark-edit-bookmark)
          ("Rename bookmark"                  . helm-bookmark-rename)
          ("Relocate bookmark"                . bookmark-relocate))))))

;; [VARCOMMENT.isearch by helm]
;; [VARIABLE.tqnr-section-mode-helm-swoop nil]
(when tqnr-section-mode-helm-swoop
  (use-package helm-swoop
    :bind
    ("M-<"     . helm-swoop-back-to-last-point)
    ("C-M-S-s" . helm-multi-swoop)

    :bind (:map helm-swoop-map
            ("M-i" . helm-multi-swoop-all-from-helm-swoop)
            ("C-r" . helm-previous-line)
            ("C-s" . helm-next-line))
    :bind (:map helm-multi-swoop-map
            ("C-r" . helm-previous-line)
            ("C-s" . helm-next-line))
    :bind (:map isearch-mode-map
            ("M-s" . helm-swoop-from-isearch))

    :config
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)

    ;; both parameter can be override when shackle is enable
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color t)

    ;; ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)

    ;; If you prefer fuzzy matching
    (setq helm-swoop-use-fuzzy-match t)
    ) ;; (use-package helm-swoop

  ;; [VARCOMMENT.do not have default value when run helm swoop]
  ;; [VARIABLE.tqnr-section-mode-helm-swoop-without-pre-input nil]
  (if tqnr-section-mode-helm-swoop-without-pre-input
    (use-package helm-swoop :bind ("C-S-s" . helm-swoop-without-pre-input))
    (use-package helm-swoop :bind ("C-S-s" . helm-swoop)))
  ) ;; (when tqnr-section-mode-helm-swoop

;; [VARCOMMENT.replace fuzzy search in find-files by flx, more human matches]
;; [VARIABLE.tqnr-section-mode-helm-flx nil]
(when tqnr-section-mode-helm-flx
  (use-package helm-flx
    :init
    (helm-flx-mode +1)
    :config
    (setq helm-flx-for-helm-find-files t)
    ) ;; (use-package helm-flx
  ) ;; (when tqnr-section-mode-helm-flx



;; [VARCOMMENT.replace yank-pop or browse kill ring by helm-kill-ring]
;; [VARIABLE.tqnr-section-mode-helm-kill-ring nil]
(when tqnr-section-mode-helm-kill-ring
  (use-package helm :bind ("M-y" . (lambda ()
                                     (interactive)
                                     (if (eq last-command 'yank)
                                       (progn
                                         (setq last-command 'yank)
                                         (yank-pop))
                                       (helm-show-kill-ring))))))

;; [VARCOMMENT.replace M-x]
;; [VARIABLE.tqnr-section-mode-helm-M-x nil]
(when tqnr-section-mode-helm-M-x
  (use-package helm :bind ("M-x" . helm-M-x)))

;; [VARCOMMENT.replace electric buffer list]
;; [VARIABLE.tqnr-section-mode-helm-buffers-list nil]
(when tqnr-section-mode-helm-buffers-list
  (use-package helm :bind
    ;; override electric-buffer-list from 08-shortcut-02-buffer
    ("C-x C-b" . helm-buffers-list)))

;; [VARCOMMENT.add helm menu shortcut for function/variable list]
;; [VARIABLE.tqnr-section-mode-helm-imenu nil]
(when tqnr-section-mode-helm-imenu
  (use-package helm :bind ("M-\\" . helm-semantic-or-imenu)))

;; search all occurrences in the current buffer (more like modern graphical editor)
;; [VARCOMMENT.replace occur by helm]
;; [VARIABLE.tqnr-section-mode-helm-occur nil]
(when tqnr-section-mode-helm-occur
  (use-package helm :bind ("C-c e" . helm-occur)))

;; [VARCOMMENT.replace find files C-x C-f]
;; [VARIABLE.tqnr-section-mode-helm-find-files nil]
(when tqnr-section-mode-helm-find-files
  (use-package helm :bind ("C-x C-f" . helm-find-files)))

;; [VARCOMMENT.replace recentf]
;; [VARIABLE.tqnr-section-mode-helm-recentf nil]
(when tqnr-section-mode-helm-recentf
  (use-package helm :bind ("C-c f" . helm-recentf)))

;; [VARCOMMENT.replace bookmark list]
;; [VARIABLE.tqnr-section-mode-helm-bookmark nil]
(when tqnr-section-mode-helm-bookmark
  (use-package helm
    :hook
    (c-mode-common-hook . (lambda () (local-unset-key (kbd "C-c C-b"))))
    (ada-mode-hook      . (lambda () (local-unset-key (kbd "C-c C-b"))))
    :bind
    ;; override bookmark-bmenu-list from 08-shortcut-02-buffer
    ("C-c C-b" . helm-bookmarks)
    ) ;; (use-package helm
  ) ;; (when tqnr-section-mode-helm-bookmark

;; [VARCOMMENT.enable Helm everywhere when asking file]
;; [VARIABLE.tqnr-section-mode-helm-global nil]
(when tqnr-section-mode-helm-global
  (use-package helm
    :init
    ;; to enable helm everywhere completing-read or read-file-name function are used
    (helm-mode 1)))


(provide '02-mode-001-helm)

;;; 02-mode-001-helm.el ends here
