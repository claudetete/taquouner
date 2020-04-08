;;; 02-mode-085-flyspell.el --- configuration of flyspell mode -*- lexical-binding: t -*-

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
;; Created: September 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [SUBHEADER.On-the-fly spell checking]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package flyspell
  ;; make sure it is loaded and custom without searched in package list
  ;; it is not listed as built-in package
  :ensure nil

  :config
  ;; [VARCOMMENT.set program to be use with ispell]
  ;; [VARIABLE.tqnr-profile-ispell-program "aspell"]
  (setq ispell-program-name tqnr-profile-ispell-program)
  ;; [VARCOMMENT.language to use with ispell]
  ;; [VARIABLE.tqnr-profile-ispell-dictionary "english"]
  (setq ispell-dictionary tqnr-profile-ispell-dictionary)
  ;; save the personal dictionary without confirmation
  (setq ispell-silently-savep t)
  ;; to speed up aspell but less accurate
  (setq ispell-extra-args '("--sug-mode=ultra"))

  ;; autoload to load aspell only when needed
  ;; if load/require it search aspell executable in 7 seconds !
  (autoload 'ispell-word "ispell"
    "Check the spelling of word in buffer." t)
  (autoload 'ispell-region "ispell"
    "Check the spelling of region." t)
  (autoload 'ispell-buffer "ispell"
    "Check the spelling of buffer." t)
  (autoload 'ispell-complete-word "ispell"
    "Look up current word in dictionary and try to complete it." t)
  (autoload 'ispell-change-dictionary "ispell"
    "Change ispell dictionary." t)
  (autoload 'ispell-message "ispell"
    "Check spelling of mail message or news post.")
  (autoload 'ispell-minor-mode "ispell"
    "Toggle mode to automatically spell check words as they are typed in.")

  ;; [VARCOMMENT.POPUP: Correct the misspelled word in popup menu]
  ;; [VARIABLE.tqnr-section-mode-flyspell-popup nil]
  (when tqnr-section-mode-flyspell-popup
    (use-package flyspell-popup
      :bind (:map flyspell-mode-map
              ("C-;" . flyspell-popup-correct))))

  ;; to avoid messages and increase performance
  (setq flyspell-issue-message-flag nil)
  ) ;; (use-package flyspell


(when (not tqnr-section-mode-hydra-spelling)
  (use-package flyspell
    ;; make sure it is loaded and custom without searched in package list
    ;; it is not listed as built-in package
    :ensure nil

    :bind
    ;; run ispell (dictionary) (set language in `section-misc')
    ("<f7>"     . ispell-buffer)
    ("<S-f7>"   . ispell-word)
    ("<M-f7>"   . ispell-region)
    ("<C-f7>"   . ispell-comments-and-strings)
    ("<C-M-f7>" . ispell-change-dictionary)
    ) ;; (use-package flyspell
  ) ;; (when (not tqnr-section-mode-hydra-spelling)


(provide '02-mode-085-flyspell)

;;; 02-mode-085-flyspell.el ends here
