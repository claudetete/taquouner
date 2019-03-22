;;; 02-mode-072-smartparens.el --- configuration of smartparens mode

;; Copyright (c) 2017-2019 Claude Tete
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
;; Version: 0.5
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.useful to have nice navigation through source code structure]
;; [SUBDEFAULT.nil]


;;; Code:
(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/smartparens-master"))
(when (try-require 'smartparens-config "    ")
  ;; enable everywhere
  (smartparens-global-mode t)
  ;; Always start smartparens mode in some major mode.
  ;;(add-hook 'c-mode-hook #'smartparens-mode)
  ;;(add-hook 'c-mode-hook #'smartparens-mode)
  ;; do not navigate between symbol only between special characters
  (setq p-navigate-consider-symbols nil)
  ;; do not consider comment as sexps
  (setq sp-navigate-comments-as-sexps nil)
  ;; replace electric pair in perl by smartparens
  (with-eval-after-load 'cperl-mode
    (add-hook 'smartparens-enabled-hook (lambda () (define-key cperl-mode-map "{" nil)))
    (add-hook 'smartparens-disabled-hook (lambda () (define-key cperl-mode-map "{" 'cperl-electric-lbrace))))
  ;; add pair < > in lisp mode
  (sp-local-pair 'emacs-lisp-mode "<" ">")
  ;; remove pair ' ' in ada mode
  (sp-local-pair 'ada-mode "'" nil :actions nil)
  ;; remove pair ' ' in minibuffer
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  ;; can be used
  ;(sp-local-pair 'fitnesse-mode "'''" "'''")

  ;; to banish smartparent for org-mode (no more used, instead restricted list of shortcuts in org-mode)
  ;; (with-eval-after-load 'org (add-to-list 'sp-ignore-modes-list #'org-mode))
  )

(with-eval-after-load 'smartparens-config
  ;; (todo when no more upper hierarchy, goes to next/previous using beginning-of-defun in c-mode or sp-backward/forward-sexp)
  (when (not (equal major-mode 'org-mode))
    ;; move to previous on same level or upper block
    (define-key smartparens-mode-map      (kbd "<M-up>")          #'sp-backward-sexp)
    ;; move to next on same level or upper block
    (define-key smartparens-mode-map      (kbd "<M-down>")        #'sp-next-sexp)
    ;; move to upper block at begin of block
    (define-key smartparens-mode-map      (kbd "<M-left>")        #'sp-backward-up-sexp)
    ;; move to downer block at begin of block
    (define-key smartparens-mode-map      (kbd "<M-right>")       #'sp-down-sexp)

    ;; move to previous on same level or upper block at end of block
    (define-key smartparens-mode-map      (kbd "<M-S-up>")        #'sp-previous-sexp)
    ;; move to next on same level or upper block at end of block
    (define-key smartparens-mode-map      (kbd "<M-S-down>")      #'sp-forward-sexp)
    ;; move to downer block at end of block
    (define-key smartparens-mode-map      (kbd "<M-S-left>")      #'sp-backward-down-sexp)
    ;; move to upper block at end of block
    (define-key smartparens-mode-map      (kbd "<M-S-right>")     #'sp-up-sexp)

    ;; move to beginning of expression just after special character
    (define-key smartparens-mode-map      (kbd "<M-home>")        #'sp-beginning-of-sexp)
    ;; move to end of expression before special character
    (define-key smartparens-mode-map      (kbd "<M-end>")         #'sp-end-of-sexp)

    ;; unwrap current block or next
    (define-key smartparens-mode-map      (kbd "<M-delete>")      #'sp-unwrap-sexp)
    ;; unwrap downer block or previous
    (define-key smartparens-mode-map      (kbd "<M-backspace>")   #'sp-backward-unwrap-sexp)

    ;; unwrap current block or next
    (define-key smartparens-mode-map      (kbd "<M-S-delete>")    #'sp-splice-sexp)
    )

  ;; include next expression in current wrap
  (define-key smartparens-mode-map      (kbd "C-(")             #'sp-backward-slurp-sexp)
  ;; move out last expression from current wrap
  (define-key smartparens-mode-map      (kbd "C-M-(")           #'sp-backward-barf-sexp)

  ;; include next expression in current wrap
  (define-key smartparens-mode-map      (kbd "C-)")             #'sp-forward-slurp-sexp)
  ;; move out last expression from current wrap
  (define-key smartparens-mode-map      (kbd "C-M-)")           #'sp-forward-barf-sexp)

  ;; M-( will enclose symbol at point with (), H-( will replace existing pair at point with ()
  (define-key smartparens-mode-map      (kbd "M-(")             (lambda () (interactive)
                                                                  (sp-wrap-with-pair "(")))
  (define-key smartparens-mode-map      (kbd "M-)")             (lambda () (interactive)
                                                                  (sp-wrap-with-pair "(")))
  (define-key smartparens-mode-map      (kbd "H-(")             (lambda () (interactive)
                                                                  (sp-rewrap-sexp '("(" . ")"))))

  ;; M-[ will enclose symbol at point with [], H-[ will replace existing pair at point with []
  (define-key smartparens-mode-map      (kbd "M-[")             (lambda () (interactive)
                                                                  (sp-wrap-with-pair "[")))
  ;;(define-key smartparens-mode-map      (kbd "M-]")             (lambda () (interactive)
  ;;                                                                (sp-wrap-with-pair "[")))
  (define-key smartparens-mode-map      (kbd "H-[")             (lambda () (interactive)
                                                                  (sp-rewrap-sexp '("[" . "]"))))

  ;; M-" will enclose symbol at point with "", H-" will replace existing pair at point with ""
  (define-key smartparens-mode-map      (kbd "M-\"")            (lambda () (interactive)
                                                                  (sp-wrap-with-pair "\"")))
  (define-key smartparens-mode-map      (kbd "H-\"")            (lambda () (interactive)
                                                                  (sp-rewrap-sexp '("\"" . "\""))))

  ;; M-' will enclose symbol at point with '', H-' will replace existing pair at point with ''
  (define-key smartparens-mode-map      (kbd "M-'")             (lambda () (interactive)
                                                                  (sp-wrap-with-pair "'")))
  (define-key smartparens-mode-map      (kbd "H-'")             (lambda () (interactive)
                                                                  (sp-rewrap-sexp '("'" . "'"))))
  ;; M-{ will enclose symbol at point with {}, H-{ will replace existing pair at point with ''
  (define-key smartparens-mode-map      (kbd "M-{")             (lambda () (interactive)
                                                                  (sp-wrap-with-pair "{")))
  (define-key smartparens-mode-map      (kbd "M-}")             (lambda () (interactive)
                                                                  (sp-wrap-with-pair "{")))
  (define-key smartparens-mode-map      (kbd "H-{")             (lambda () (interactive)
                                                                  (sp-rewrap-sexp '("{" . "}"))))

  ;;(define-key smartparens-mode-map      (kbd "<S-home>")        'sp-end-of-previous-sexp)
  ;;(define-key smartparens-mode-map      (kbd "<S-end>")         'sp-beginning-of-next-sexp)
  ;;(define-key smartparens-mode-map      (kbd "<S-prior>")       'sp-beginning-of-previous-sexp)
  ;;(define-key smartparens-mode-map (kbd "") 'sp-end-of-next-sexp (&optional arg)             ;; none
  ) ;; (with-eval-after-load 'smartparens-config


(provide '02-mode-072-smartparens)

;;; 02-mode-072-smartparens.el ends here
