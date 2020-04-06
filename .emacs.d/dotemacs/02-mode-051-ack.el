;;; 02-mode-051-ack.el --- configuration of Ack mode

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
;; Version: 0.2
;; Created: July 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.search with ack (no more grep) (need perl interpreter)]
;; [SUBDEFAULT.nil]
;; deprecated, use ag/pt/rg


;;; Code:

;;
;; search with ack (no more grep)
;; need a patched ack (patch from https://github.com/blixtor/ack/commit/e9ee7ff0e32da86011418dcb9d52c25b1b6d8bdb by blixtor)
;; the ack-standalone present in .emacs.d/plugins/ folder is already patched

;; [VARCOMMENT.Full-ack mode to interface ack with emacs]
;; [VARIABLE.tqnr-section-mode-ack-full nil]
(when tqnr-section-mode-ack-full
  (autoload 'ack-same "full-ack" nil t)
  (autoload 'ack "full-ack" nil t)
  (autoload 'ack-find-same-file "full-ack" nil t)
  (autoload 'ack-find-file "full-ack" nil t)

  (setq ack-executable (concat "perl " (file-name-as-directory tqnr-dotemacs-path) "plugins/ack-standalone"))
  (setq ack-prompt-for-directory t)
  )

;; [VARCOMMENT.ack and half mode to interface ack with emacs]
;; [VARIABLE.tqnr-section-mode-ack-and-half nil]
(when tqnr-section-mode-ack-and-half
  (autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
  (autoload 'ack-and-a-half "ack-and-a-half" nil t)
  (autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
  (autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
  )

;; [VARCOMMENT.ack-emacs mode to interface ack with emacs]
;; [VARIABLE.tqnr-section-mode-ack-emacs nil]
(when tqnr-section-mode-ack-emacs
  (try-require 'autoload-ack-emacs "    ")
  (setq ack-command (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/ack-standalone"))

  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; run ack with thing at point
      (global-set-key   (kbd "<C-f3>")  'ack)
      ;; run ack with thing at point but only with same file type
      (global-set-key   (kbd "<M-f3>")  'ack-same)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )


(provide '02-mode-051-ack)

;;; 02-mode-051-ack.el ends here
