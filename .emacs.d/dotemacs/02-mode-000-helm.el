;;; 02-mode-000-helm.el --- configuration of helm mode

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
;; [SUBHEADER.(fork ANYTHING) choose anything with the same nice interface]

;;; Change Log:
;; 2017-07-24 (0.1)
;;    creation from split of old mode.el (see 02-mode.el for history)


;;; Code:

;;
(add-to-list 'load-path  (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/helm-master"))

(custom-set-variables
  '(helm-follow-mode-persistent t))

(when (try-require 'helm-config "    ")
  (when tqnr-running-on-ms-windows
    ;; configure everything instead of locate under MS Windows
    ;; client must be started in background before use
    ;; $ Everything.exe -startup
    (setq helm-locate-command "es -s %s -sort run-count %s")
    (setq helm-locate-fuzzy-match nil)
    (defun helm-es-hook ()
      (when (and (equal (assoc-default 'name (helm-get-current-source)) "Locate")
              (string-match "\\`es" helm-locate-command))
        (mapc (lambda (file)
                (call-process "es" nil nil nil
                  "-inc-run-count" (convert-standard-filename file)))
          (helm-marked-candidates))))
    (add-hook 'helm-find-many-files-after-hook 'helm-es-hook)
    )
  (setq helm-candidate-separator
    "--separator------------------------------")
  (when tqnr-section-mode-cedet-ecb
    ;; quit helm when hide compile window
    (add-hook 'ecb-toggle-compile-hide-hook 'helm-keyboard-quit))
  (when tqnr-section-mode-helm-buffers-list
    ;; to avoid error with helm-buffers-list
    (setq ido-use-virtual-buffers nil))
  (when (and tqnr-section-mode-helm-bookmark tqnr-section-mode-helm-find-files)
    (custom-set-variables
      '(helm-type-bookmark-actions
         (quote
           (("Jump to bookmark with find-files" . helm-bookmark-find-files-jump)
             ("Jump to bookmark"                . helm-bookmark-jump)
             ("Jump to BM other window"         . helm-bookmark-jump-other-window)
             ("Bookmark edit annotation"        . bookmark-edit-annotation)
             ("Bookmark show annotation"        . bookmark-show-annotation)
             ("Delete bookmark(s)"              . helm-delete-marked-bookmarks)
             ("Edit Bookmark"                   . helm-bookmark-edit-bookmark)
             ("Rename bookmark"                 . helm-bookmark-rename)
             ("Relocate bookmark"               . bookmark-relocate))))))
  )


(with-eval-after-load "helm"
  ;; quit helm with ESCAPE
  (define-key helm-map  (kbd "ESC")             'helm-keyboard-quit)
  ;; quit helm with f2
  (define-key helm-map  (kbd "<f2>")            'helm-keyboard-quit)
  ;; go to beginning of buffer
  (define-key helm-map  (kbd "M-<home>")        'helm-beginning-of-buffer)
  (define-key helm-map  (kbd "C-<home>")        'helm-beginning-of-buffer)
  ;; go to end of buffer
  (define-key helm-map  (kbd "M-<end>")         'helm-end-of-buffer)
  (define-key helm-map  (kbd "C-<end>")         'helm-end-of-buffer)
  ;; show previous element in history
  (define-key helm-map  (kbd "M-<up>")          'previous-history-element)
  ;; show next element in history
  (define-key helm-map  (kbd "M-<down>")        'next-history-element)
  ) ; (with-eval-after-load "helm"

;;
;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; [VARCOMMENT.replace yank-pop or browse kill ring by helm-kill-ring]
    ;; [VARIABLE.tqnr-section-mode-helm-kill-ring nil]
    (when tqnr-section-mode-helm-kill-ring
      (global-set-key     (kbd "M-y")             (lambda ()
                                                    (interactive)
                                                    (if (eq last-command 'yank)
                                                      (progn
                                                        (setq last-command 'yank)
                                                        (yank-pop))
                                                      (helm-show-kill-ring)))))
    ;; [VARCOMMENT.replace M-x]
    ;; [VARIABLE.tqnr-section-mode-helm-M-x nil]
    (when tqnr-section-mode-helm-M-x
      (global-set-key     (kbd "M-x")             'helm-M-x))

    ;; [VARCOMMENT.replace electric buffer list]
    ;; [VARIABLE.tqnr-section-mode-helm-buffers-list nil]
    (when tqnr-section-mode-helm-buffers-list
      (global-set-key     (kbd "M-z")             'helm-buffers-list)
      ;; override electric-buffer-list from 08-shortcut-02-buffer
      (global-set-key     (kbd "C-x C-b")         'helm-buffers-list)
      )

  ;; [VARCOMMENT.add helm menu shortcut for function/variable list]
  ;; [VARIABLE.tqnr-section-mode-helm-imenu nil]
  (when tqnr-section-mode-helm-imenu
    (global-set-key     (kbd "M-\\")            'helm-semantic-or-imenu))

    ;; search all occurrences in the current buffer (more like modern graphical editor)
    ;; [VARCOMMENT.replace occur by helm]
    ;; [VARIABLE.tqnr-section-mode-helm-occur nil]
    (when tqnr-section-mode-helm-occur
      (global-set-key           (kbd "C-c e")           'helm-occur))

    ;; [VARCOMMENT.replace find files C-x C-f]
    ;; [VARIABLE.tqnr-section-mode-helm-find-files nil]
    (when tqnr-section-mode-helm-find-files
      (global-set-key           (kbd "C-x C-f")         'helm-find-files))

    ;; [VARCOMMENT.replace recentf]
    ;; [VARIABLE.tqnr-section-mode-helm-recentf nil]
    (when tqnr-section-mode-helm-recentf
      (global-set-key           (kbd "C-c f")           'helm-recentf))

    ;; [VARCOMMENT.replace bookmark list]
    ;; [VARIABLE.tqnr-section-mode-helm-bookmark nil]
    (when tqnr-section-mode-helm-bookmark
      (add-hook 'c-mode-common-hook '(lambda () (local-unset-key (kbd "C-c C-b"))))
      ;; override bookmark-bmenu-list from 08-shortcut-02-buffer
      (global-set-key           (kbd "C-c C-b")         'helm-bookmarks)
      )

    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '02-mode-000-helm)

;;; 02-mode-000-helm.el ends here
