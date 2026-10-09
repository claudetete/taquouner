;;; 02-mode-082-shackle.el --- configuration of arduino mode -*- lexical-binding: t -*-

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
;; Created: August 2017
;; Last-Updated: April 2020

;;; Commentary:
;;
;; [[SUBHEADER.mode to have popup always following same rules
;; like popwin but just add constraint to popup not replace the whole thing
;; Helm does not like popwin...
;; Can be used with popper to have same behavior than popwin
;; ]]
;; [SUBDEFAULT.nil]


;;; Code:
(use-package shackle
  :init
  ;; enable shackle mode
  (shackle-mode 1)

  :config
  (setq shackle-default-size 0.2)
  ;; rules to add helm
  (setq shackle-rules
    '(
       (compilation-mode                        :align t               :size 0.3 :select nil)
       ("*Shell Command Output*"                :align t                         :select nil)
       (dired-mode                              :align t)
       ("*Messages*"                            :align t)
       ("*Calculator*"                          :align t)
       ("*vc*"                                  :align t)
       ("*vc-diff*"                             :align t)
       ("*Apropos*"                             :align t)
       ("*Occur*"                               :align t)
       ("*shell*"                               :align t)
       ("*Help*"                                :align t)
       ("*Backtrace*"                           :align t)
       ("*Compile-log*"                         :align t)
       ("*[Ss]ynergy*"                          :align t     :regexp t)
       ("\\s-*\\*[Ss]ynergy.*\\*\\s-*"          :align t     :regexp t)
       ("\\s-*\\*[cC]ompletions*\\*\\s-*"       :align below :regexp t)
       ("\\*[cC]ompilation.*\\*"                :align t     :regexp t)
       ("\\*i?grep.*\\*"                        :align t     :regexp t)
       ("*JDEE Compile Server*"                 :align t)
       ("*ccm*"                                 :align t)
       ("\\*GTAGS SELECT\\*.*"                  :align t     :regexp t)
       ("*Bookmark List*"                       :align t)
       ("*Semantic Context Analyze*"            :align t)
       ("*Macroexpansion*"                      :align t)
       ("\\*Symref .*"                          :align t     :regexp t)
       ("*ECB Analyse*"                         :align t)
       ("*Kill Ring*"                           :align t)
       ("*clearcase-config-spec*"               :align t)
       ("*ELP Profiling Results*"               :align t)
       ("$\\s-*\\*[mM]agit.*\\*\\s-*"           :align t     :regexp t)
       ("*pt-search*"                           :align t)
       ("$\\s-*\\*[Aa]ck\\*\\s-*"               :align t     :regexp t)
       ("$\\s-*\\*[Gg]tags.*\\*"                :align t     :regexp t)
       ("\\s-*\\*undo-tree.*"                   :align right :regexp t :size 0.1)
       ("*Python Doc*"                          :align t)
       ("*pydoc*"                               :align t)
       ("*eldoc*"                               :align t)
       ("*haskell*"                             :align t)
       ("*debug:haskell*"                       :align t)
       ("*magit async*"                         :align t               :size 0.1)
       ("\\s-*\\*magit.*\\*\\s-*"               :align t     :regexp t :size 0.3)
       ("\\s-*magit-process:.*"                 :align t     :regexp t :size 0.3)
       ("\\s-*magit-log:.*"                     :align t     :regexp t :size 0.3)
       ("\\s-*magit:.*"                         :align t     :regexp t :size 0.3)
       (tqnr-ada-compile-buffer-name            :align t               :size 0.3)
       (tqnr-ada-check-buffer-name              :align t               :size 0.3)
       (tqnr-ada-pretty-print-buffer-name       :align t               :size 0.3)
       ("\\s-*\\*helpful.*\\*\\s-*"             :align t     :regexp t)
       ("\\s-*\\*i?pdb.*\\*\\s-*"               :align t     :regexp t)
       ("\\s-*\\*Embark Actions.*"              :align t     :regexp t :size 0.5 :select t)
       ("\\s-*\\*Embark Collect:.*"             :align t     :regexp t :size 0.25 :select t)
       ("\\s-*\\*Embark Export:.*"              :align t     :regexp t :size 0.25 :select t)
       ;; use 25% at bottom
       ;; all helm buffer
       ("\\`\\*helm.*?\\*\\'"                   :align t     :regexp t :size 0.25)
       ;; helm swoop buffer
       ("\\`\\*Helm.*?\\*\\'"                   :align t     :regexp t :size 0.25)
       ;;
       ;("\\`\\*Embark.*?\\*\\'"                :align t     :regexp t :size 0.25)
       ("*EGLOT LSP Hierarchy*"                 :align t               :size 0.3 :select t)
       ("\\s-*\\*eldoc.*\\*\\s-*"               :align t     :regexp t)
       ("\\s-*\\*xref\\*\\s-*"                  :align t     :regexp t)
       ("*Warnings*"                            :align t)
       ))
  ) ;; (use-package shackle


(provide '02-mode-082-shackle)

;;; 02-mode-082-shackle.el ends here
