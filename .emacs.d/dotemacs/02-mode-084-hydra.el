;;; 02-mode-084-hydra.el --- configuration of hydra, new ways to bind

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
;; Version: 0.4
;; Created: September 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.Create families of short bindings with a common prefix]
;; [SUBDEFAULT.t]


;;; Code:

(add-to-list 'load-path (concat (file-name-as-directory tqnr-dotemacs-path) "plugins/hydra-master"))

(when (try-require 'hydra "      ")

  ;;
  ;; RECTANGLE
  ;;
  ;; [VARCOMMENT.Use Hydra to manage rectangle shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-rectangle t]
  (when tqnr-section-mode-hydra-rectangle
    ;; docstring inspired by https://oremacs.com/2015/04/15/hydra-idle-hint/
    (when (try-require 'rect "        ")
      (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                  :color pink
                                  :hint nil
                                  :post (deactivate-mark))
      "
╭─^───^─────┐
│R^ect^angle│    I^n^sertion       ^  O^thers               [_q_] Quit
└─^───^─────╯──┬──^─^───────────┬──^───^───────────────────────────╮
  [_M-w_] Copy   [_s_]   String   [_<delete>_] Delete
  [_C-w_] Kill   [_SPC_] String   [_S-SPC_]    Replace by space
  [_C-y_] Yank   [_n_]   Number   [_M-SPC_]    Delete only space
   ^   ^          ^ ^             [_<return>_] Cycle corner cursor

"
        ;; Copy/Paste
        ("M-w"        copy-rectangle-as-kill)
        ("C-w"        kill-rectangle)
        ("C-y"        yank-rectangle :color blue)
        ("<S-insert>" yank-rectangle :color blue)
        ;; Insertion
        ("s"          string-rectangle :color blue)
        ("SPC"        string-rectangle :color blue)
        ("<C-return>" open-rectangle :color blue)
        ("n"          rectangle-number-lines-wrapper :color blue)
        ;; Others
        ("<delete>"    delete-rectangle :color blue)
        ("<backspace>" delete-rectangle :color blue)
        ("S-SPC"       clear-rectangle :color blue)
        ("M-SPC"       delete-whitespace-rectangle :color blue)
        ("<return>"    rectangle-exchange-point-and-mark)
        ;; with hint
        ("r" (if (region-active-p)
               (deactivate-mark)
               (rectangle-mark-mode 1)) "Reset")
        ("q" nil "Quit" :color blue)
        )
      ;; shortcuts are put in a hook to be loaded after everything else in init process
      (add-hook 'tqnr-after-init-shortcut-hook
        (lambda ()
          (global-set-key   (kbd "<C-return>")    'hydra-rectangle/body)
          (global-set-key   (kbd "C-x SPC")       'hydra-rectangle/body)
          ) ;; (lambda ()
        ) ;; (add-hook 'tqnr-after-init-shortcut-hook
      ) ;; (when (try-require 'rect "        ")
    ) ;; (when tqnr-section-mode-hydra-rectangle


  ;;
  ;; DISPLAY
  ;;
  ;; [VARCOMMENT.Use Hydra to manage windows/frame/buffer shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-display t]
  (when tqnr-section-mode-hydra-display
    ;; need to unbind them to be able to bind them into hydra
    (define-key hydra-base-map    (kbd "<kp-0>")        nil)
    (define-key hydra-base-map    (kbd "<kp-2>")        nil)
    (define-key hydra-base-map    (kbd "<kp-6>")        nil)
    (define-key hydra-base-map    (kbd "<kp-subtract>") nil)
    (defhydra hydra-display (:exit t
                              :color pink
                              :hint nil)
    "
╭─^─────^─^┐
│D^ispla^y^│  Wi^n^dow   ^ ^      ^ ^      ^   ^ Frame^ ^      ^   ^ Buffe^r^      ^  ^      ^
└─^─────^─^╯────^─^──────^─^──────^─^──────^─┬─^──────^─^──────^─┬─^──────^─^──────^──^──────^───^─^─^─^─^─^─╮
  ^     ^ ^     ^ ^╭─┬─╮ ^ ^╭─┬─╮ ^ ^╭─┬─╮ ^ ┆ ^      ^ ^      ^ ┆ ^╭─┬─╮ ^ ^╭─┬─╮ ^  ^╭─┬─╮ ^
  ^╭───╮^ ^╭─┬─╮^ ^│▤├─┤ ^ ^│▤├─┤ ^ ^│ │ │ ^ ┆ ^╭───╮ ^ ^╭───╮ ^ ┆ ^│▤├─┤ ^ ^│▤├─┤ ^  ^│▤│ │ ^   ^ ^ _↑_
  ^├───┤^ ^│ │ │^ ^╰╭───╮^ ^╰╭───╮^ ^╰╭───╮^ ┆ ^│╭──┴╮^ ^│╭──┴╮^ ┆ ^╰╭─┬─╮^ ^╰╭───╮^  ^╰╭─┬─╮^   _←_ ^ ^ _→_
  ^╰───╯^ ^╰─┴─╯^ ^ │▤  │^ ^ ├───┤^ ^ ├───┤^ ┆ ^╰│ ✚ │^ ^╰│ ✖ │^ ┆ ^ │ ├─┤^ ^ ├───┤^  ^ │ │▤│^   ^ ^ _↓_
  ^     ^ ^     ^ ^ ╰───╯^ ^ ╰───╯^ ^ ╰───╯^ ┆ ^ ╰───╯^ ^ ╰───╯^ ┆ ^ ╰─┴─╯^ ^ ╰───╯^  ^ ╰─┴─╯^
   [_2_]   [_6_]    [_0_]    [_._]   [_|_]   ┆  [_+_]    [_-_]   ┆  [_k_]     [_b_]    [_~_] ^    Move

"
      ;; Window
      ("2" (progn
             (split-window-below)
             (windmove-down 0)))
      ("6" (progn
             (split-window-right)
             (windmove-right 0)))
      ("0" delete-other-windows)
      ("." delete-window)
      ("|" (when tqnr-section-function-buffer-window
             (my-toggle-window-split))
        "Toggle window split")
      ;; Frame
      ("+" make-frame-command)
      ("-" delete-frame)
      ;; Buffer
      ;; Buffer
      ("k" kill-this-buffer "Kill buffer")
      ("b" kill-buffer-and-window "Kill buffer and window")
      ("~" (when tqnr-section-function-buffer-window
             (my-swap-windows))
        "Swap windows")
      ("<up>"      (ignore-errors (windmove-up)) nil :color red)
      ("<S-up>"    (ignore-errors (windmove-up)) nil :color red)
      ("<down>"    (ignore-errors (windmove-down)) nil :color red)
      ("<S-down>"  (ignore-errors (windmove-down)) nil :color red)
      ("<left>"    (ignore-errors (windmove-left)) nil :color red)
      ("<S-left>"  (ignore-errors (windmove-left)) nil :color red)
      ("<right>"   (ignore-errors (windmove-right)) nil :color red)
      ("<S-right>" (ignore-errors (windmove-right)) nil :color red)
      ("<next>" (when tqnr-section-function-buffer-window
                  (scroll-up-keep-cursor)) "Scroll Down" :color red)
      ("<prior>" (when tqnr-section-function-buffer-window
                   (scroll-down-keep-cursor)) "Scroll Up" :color red)
      )
    ;; rebind them after definition
    (define-key hydra-base-map    (kbd "<kp-0>")          'hydra--digit-argument)
    (define-key hydra-base-map    (kbd "<kp-2>")          'hydra--digit-argument)
    (define-key hydra-base-map    (kbd "<kp-6>")          'hydra--digit-argument)
    (define-key hydra-base-map    (kbd "<kp-subtract>")   'hydra--negative-argument)
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "<M-kp-divide>") 'hydra-display/body)
        (global-set-key   (kbd "M-z")           'hydra-display/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-display


  ;;
  ;; TRANSPOSE
  ;;
  ;; [VARCOMMENT.Use Hydra to manage transpose shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-transpose t]
  (when tqnr-section-mode-hydra-transpose
    (defhydra hydra-transpose (:color red
                                :hint nil)
    "
╭──^─^──────┐
│Tr^a^nspose│         ^ ^       [_q_] Quit
└──^─^──────╯──────┬──^─^──────────────╮
  [_c_] Character    [_p_] Paragraph
  [_w_] Word         [_o_] Org word
  [_x_] Expression   [_e_] Org element
  [_l_] Line         [_y_] Org table
  [_s_] Sentence
"
      ("c" transpose-chars)
      ("t" transpose-chars)
      ("w" transpose-words)
      ("x" (if tqnr-section-mode-smartparens
             (sp-transpose-sexp)
             (transpose-sexps)))
      ("l" transpose-lines)
      ("s" transpose-sentences)
      ("p" transpose-paragraphs)
      ("o" org-transpose-words)
      ("e" org-transpose-elements)
      ("y" org-table-transpose-table-at-point)
      ("q" nil nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "C-t") 'hydra-transpose/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-transpose


  ;;
  ;; HELP/WEB
  ;;
  ;; [VARCOMMENT.Use Hydra to manage help/web shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-help-web t]
  (when tqnr-section-mode-hydra-help-web
    (defhydra hydra-help-web (:color pink
                               :hint nil)
    "
╭──^─^─────┐
│He^l^p/Web│         ^ ^                        ^ ^        [_q_] Quit
└──^─^─────╯──────┬──^─^─────────────────────┬──^─^───────────────╮
  [_f_] Function   [_e_] WordReference EnFr   [_g_] Google Fr
  [_k_] Key        [_r_] WordReference FrEn   [_o_] Google En
  [_a_] Apropos    [_s_] Synonyme Fr          [_w_] Wikipedia Fr
  [_b_] Binding    [_y_] Synonyme En          [_i_] Wikipedia En
  [_v_] Variable   [_c_] Conjugation Fr       [_d_] Duckduckgo
   ^ ^             [_j_] Conjugation En
  Capitalized to use default help instead of helpful.
"
      ;; Help
      ("f" (if tqnr-section-mode-helpful
             (helpful-callable)
             (describe-function)) :color blue)
      ("F" describe-function :color blue)
      ("k" (if tqnr-section-mode-helpful
             (helpful-key)
             (describe-key)) :color blue)
      ("K" describe-key :color blue)
      ("a" (if tqnr-section-mode-helpful
             (helpful-command)
             (apropos-command)) :color blue)
      ("A" apropos-command :color blue)
      ("b" describe-bindings :color blue)
      ("v" (if tqnr-section-mode-helpful
             (helpful-variable)
             (describe-variable)) :color blue)
      ("V" describe-variable :color blue)
      ;; Spelling
      ("e" translate-fren :color blue)
      ("r" translate-enfr :color blue)
      ("s" synonym-fr :color blue)
      ("y" synonym-en :color blue)
      ("c" conjugation-fr :color blue)
      ("j" conjugation-en :color blue)
      ;; Web
      ("g" google-fr :color blue)
      ("o" google-en :color blue)
      ("w" wikipedia-fr :color blue)
      ("i" wikipedia-en :color blue)
      ("d" duckduckgo :color blue)
      ;; Quit
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "C-?") 'hydra-help-web/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-help


  ;;
  ;; MACRO
  ;;
  ;; [VARCOMMENT.Use Hydra to manage macro shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-macro t]
  (when tqnr-section-mode-hydra-macro
    (defhydra hydra-macro (:color pink
                            :hint nil
                            :pre (when defining-kbd-macro
                                   (kmacro-end-macro 1)))
      "
╭──^───┐
│Ma^cro│^                ^ ^              ^      ^      [_q_] Quit
└──^───╯^────────────────^─^──────────────^──────^─────────────╮
  [_<f8>_] Start/Stop   [_s_]   Save     [_<H-up>_]   Previous
  [_r_]    Run last     [_DEL_] Delete   [_<H-down>_] Next
  [_e_]    Edit         [_i_]   Edit step

"
      ;;
      ("<f8>" kmacro-start-macro :color blue)
      ("r"    call-last-kbd-macro-region)
      ;;
      ("s"   kmacro-name-last-macro)
      ("DEL" kmacro-delete-ring-head)
      ("e"   kmacro-edit-macro-repeat)
      ("i"   kmacro-step-edit-macro)
      ;;
      ("<H-up>"   kmacro-cycle-ring-previous)
      ("<H-down>" kmacro-cycle-ring-next)
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "<f8>")    'hydra-macro/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-macro


  ;;
  ;; SPELLING
  ;;
  ;; [VARCOMMENT.Use Hydra to manage spelling shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-spelling t]
  (when tqnr-section-mode-hydra-spelling
    (defhydra hydra-spelling (:color pink
                               :hint nil)
      "
╭──^────^──┐
│Sp^elli^ng│              ^ ^                 [_q_] Quit
└──^────^──╯──────────────^─^────────────────────────╮
  [_<f7>_] Word/Region     [_<M-up>_]   Previous error
  [_b_]    Buffer          [_<M-down>_] Next error
  [_f_]    Flycheck        [_p_]        Comment/String
  [_c_]    Correct Popup   [_d_]        Dictionary
"
      ;;
      ("<f7>" ispell-word :color blue)
      ("b"    ispell-buffer :color blue)
      ("f"    flyspell-buffer)
      ("c"    flyspell-popup-correct)
      ;;
      ("<M-up>"   flycheck-previous-error)
      ("<M-down>" flycheck-next-error)
      ("p"        ispell-comments-and-strings)
      ("d"        ispell-change-dictionary)
      ;;
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "<f7>")    'hydra-spelling/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-spelling


  ;;
  ;; SEARCH
  ;;
  ;; [VARCOMMENT.Use Hydra to manage search shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-search t]
  (when tqnr-section-mode-hydra-search
    (defhydra hydra-search (:color pink
                             :hint nil)
      "
╭─^───^──┐
│S^ear^ch│                  ^ ^                           ^ ^         [_q_] Quit
└─^───^──╯──────────────────^─^───────────────────────────^─^────────────────╮
  [_s_] Isearch            [_z_]    Swoop Helm           [_]_] Match keyword
  [_S_] Isearch regexp     [_<f3>_] RipGrep
  [_w_] Isearch at point   [_g_]    Grep                 [_._] Gtags
  [_r_] Isearch backward   [_p_]    Projectile project   [_,_] Gtags pattern
  [_e_] Occur              [_f_]    Projectile file      [_<_] Gtags back
  [_E_] Occur at point     [_F_]    Projectile RipGrep
"
      ;;
      ("s" (if (bound-and-true-p isearch-mode)
             (isearch-repeat-forward)
             (isearch-forward)))
      ("S" isearch-forward-regexp :color blue)
      ("r" (if (bound-and-true-p isearch-mode)
             (isearch-repeat-backward)
             (isearch-backward)))
      ("w" isearch-forward-at-point)
      ("e" (if tqnr-section-mode-helm-occur
             (helm-occur)
             (occur))
        :color blue)
      ("E" (if tqnr-section-mode-helm-occur
             (helm-swoop)
             (occur-word-at-point))
        :color blue)
      ("]" find-matching-keyword)
      ;;
      ("z" (if tqnr-section-mode-helm-swoop-without-pre-input
             (helm-swoop-without-pre-input)
             (helm-swoop))
        :color blue)
      ("<f3>" (when tqnr-section-mode-helm
                (helm-do-ag))
        :color blue)
      ("g" grep)
      ("p" (if tqnr-section-mode-helm
             (helm-projectile-switch-project)
             (projectile-switch-project))
        :color blue)
      ("f" (if tqnr-section-mode-helm
             (helm-projectile-find-file-dwim)
             (projectile-find-file-dwim))
        :color blue)
      ("F" (if tqnr-section-mode-helm
             (helm-projectile-find-file-dwim)
             (projectile-find-file-dwim))
        :color blue)
      ;;
      ("." (if tqnr-section-mode-helm
             (helm-gtags-dwim)
             (ggtags-find-tag-dwim))
        :color blue)
      ("," (if tqnr-section-mode-helm
             (helm-gtags-find-pattern)
             (ggtags-find-reference))
        :color blue)
      ("<" (if tqnr-section-mode-helm
             (helm-gtags-pop-stack)
             (semantic-pop-tag-mark))
        :color blue)
      ;;
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "<f3>")    'hydra-search/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-macro


  ;;
  ;; SMARTPARENS
  ;;
  ;; [VARCOMMENT.Use Hydra to manage smartparens shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-smartparens t]
  (when (and tqnr-section-mode-hydra-smartparens tqnr-section-mode-smartparens)
    (defhydra hydra-smartparens (:color pink
                                  :hint nil)
      "
╭──^────────^─┐
│Sm^artparen^s│ M^ove at^ beg^in       ^    M^ove Insi^de                             [_q_] Quit
└──^────────^─╯──^──────^────^─────────^──┬──^────────^──────────────────────────────────────╮
   ^        ^   [_<M-up>_]   ^         ^    [_<M-home>_] Begin
   ^        ^    ^  ↑   ^    ^         ^    [_<M-end>_]  End
  [_<M-left>_]   ^←   → ^   [_<M-right>_]
   ^        ^    ^  ↓   ^    ^         ^    [_<M-delete>_]    Delete pair down or next
   ^        ^  [_<M-down>_]  ^         ^    [_<M-backspace>_] Delete pair current or previous
   ^  + Shif^t t^o move a^t e^nd
"
      ;; Move In/Out
      ("<M-up>"    sp-backward-sexp)
      ("<M-down>"  sp-next-sexp)
      ("<M-left>"  sp-backward-up-sexp)
      ("<M-right>" sp-down-sexp)
      ;; Move on Same Level
      ("<M-S-left>"  sp-backward-down-sexp)
      ("<M-S-right>" sp-up-sexp)
      ("<M-S-up>"    sp-previous-sexp)
      ("<M-S-down>"  sp-forward-sexp)
      ;; Move Inside
      ("<M-home>"      sp-beginning-of-sexp)
      ("<M-end>"       sp-end-of-sexp)
      ("<M-delete>"    sp-unwrap-sexp)
      ("<M-backspace>" sp-backward-unwrap-sexp)
      ;;
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key         (kbd "C-c s")   'hydra-smartparens/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-spelling


  ;;
  ;; ADA
  ;;
  ;; [VARCOMMENT.Use Hydra to manage ada compile shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-ada t]
  (when (and tqnr-section-mode-hydra-ada tqnr-section-mode-ada tqnr-section-function-ada)
    (defhydra hydra-ada (:color pink
                          :hint nil)
      "
╭──^──────^─┐
│Ada Compile│ Current file   P^r^oject    [_q_] Quit
└──^──────^─╯──────────────┬──^─^────────────────╮
  [_<f10>_] Build            [_b_] Build All
  [_p_] Pretty printer       [_c_] Clean All
  [_s_] Check syntax         [_n_] Native build
"
      ;; Move In/Out
      ("<f10>" ada-gps-build)
      ("p"     ada-gps-pretty-print)
      ("s"     ada-gps-check)
      ("b"     ada-gps-build-all)
      ("c"     ada-gps-clean-all)
      ("n"     ada-gps-build-native)
      ;;
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (with-eval-after-load "ada-mode"
          (define-key ada-mode-map      (kbd "<f10>")   'hydra-ada/body)
          ) ;; (with-eval-after-load "ada-mode"
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-spelling


  ;;
  ;; OUTLINE
  ;;
  ;; [VARCOMMENT.Use Hydra to manage outline shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-outline nil]
  (when (and tqnr-section-mode-hydra-outline tqnr-section-mode-outline)
    (defhydra hydra-outline (:color pink :hint nil)
      "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree
"
      ;; Hide
      ("q" hide-sublevels)    ; Hide everything but the top-level headings
      ("t" hide-body)         ; Hide everything but headings (all body lines)
      ("o" hide-other)        ; Hide other branches
      ("c" hide-entry)        ; Hide this entry's body
      ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
      ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
      ;; Show
      ("a" show-all)          ; Show (expand) everything
      ("e" show-entry)        ; Show this heading's body
      ("i" show-children)     ; Show this heading's immediate child sub-headings
      ("k" show-branches)     ; Show all sub-headings under this heading
      ("s" show-subtree)      ; Show (expand) everything in this heading & below
      ;; Move
      ("u" outline-up-heading)                ; Up
      ("n" outline-next-visible-heading)      ; Next
      ("p" outline-previous-visible-heading)  ; Previous
      ("f" outline-forward-same-level)        ; Forward - same level
      ("b" outline-backward-same-level)       ; Backward - same level
      ("z" nil "leave"))

    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "C-c h")    'hydra-outline/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-outline

  ) ;; (when (try-require 'hydra "      ")


  ;;
  ;; ORG MOTION/HINT
  ;;
  ;; [VARCOMMENT.Use Hydra to manage org shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-org-mode t]
  (when (and tqnr-section-mode-hydra-org-mode tqnr-section-mode-org-mode)
    (defhydra hydra-org-mode (:color pink
                               :hint nil)
      "
╭──^──────^┐
│Or^g Mode^│  ^ Move^     ^Hint    ^                              [_q_] Quit
└──^──────^╯──^─────^──┬──^────────^──────────────────────────────────────╮
   ^      ^   [_<up>_]
   ^      ^    ^ ↑  ^
  [_<left>_]   ^← → ^
   ^      ^    ^ ↓  ^
   ^      ^  [_<down>_]
"
      ;; Move Visible
      ("<left>"   outline-up-heading)
      ("<up>"     org-previous-visible-heading)
      ("<down>"   org-next-visible-heading)
      ;; Move on Same Level (even not visible)
      ("<M-up>"   org-backward-same-level)
      ("<M-down>" org-forward-same-level)
      ;;
      ("q" nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (with-eval-after-load "org-mode"
          (define-key org-mode-map      (kbd "C-c C-J")         'hydra-org-mode/body)
          ) ;; (with-eval-after-load "org-mode"
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-mode-hydra-spelling


  ;;
  ;; TOGGLE SPECIAL BUFFER
  ;;
  ;; [VARCOMMENT.Use Hydra to manage special buffer toggle shortcuts]
  ;; [VARIABLE.tqnr-section-mode-hydra-special-buffer t]
  (when tqnr-section-function-buffer-window
    (defhydra hydra-special-buffer (:color blue
                                     :hint nil)
    "
╭──^─^───────────┐
│Sp^e^cial Buffer│   [_q_] Quit
└──^─^───────────╯───────────╮
  [_b_] Bookmark        (M-2)
  [_s_] Search          (M-3)
  [_c_] Compile         (M-4)
  [_g_] Version Control (M-5)
  [_h_] Help            (M-6)
"
      ("b" toggle-bookmark-buffer)
      ("s" toggle-search-buffer)
      ("c" toggle-compilation-buffer)
      ("g" toggle-vc-buffer)
      ("h" toggle-help-buffer)
      ("q" nil nil :color blue)
      )
    ;; shortcuts are put in a hook to be loaded after everything else in init process
    (add-hook 'tqnr-after-init-shortcut-hook
      (lambda ()
        (global-set-key   (kbd "M-1") 'hydra-special-buffer/body)
        ) ;; (lambda ()
      ) ;; (add-hook 'tqnr-after-init-shortcut-hook
    ) ;; (when tqnr-section-function-buffer-window


;;
;; UNICODE
;;
;; U+2500	─	e2 94 80	BOX DRAWINGS LIGHT HORIZONTAL
;; U+2501	━	e2 94 81	BOX DRAWINGS HEAVY HORIZONTAL
;; U+2502	│	e2 94 82	BOX DRAWINGS LIGHT VERTICAL
;; U+2503	┃	e2 94 83	BOX DRAWINGS HEAVY VERTICAL
;; U+2504	┄	e2 94 84	BOX DRAWINGS LIGHT TRIPLE DASH HORIZONTAL
;; U+2505	┅	e2 94 85	BOX DRAWINGS HEAVY TRIPLE DASH HORIZONTAL
;; U+2506	┆	e2 94 86	BOX DRAWINGS LIGHT TRIPLE DASH VERTICAL
;; U+2507	┇	e2 94 87	BOX DRAWINGS HEAVY TRIPLE DASH VERTICAL
;; U+2508	┈	e2 94 88	BOX DRAWINGS LIGHT QUADRUPLE DASH HORIZONTAL
;; U+2509	┉	e2 94 89	BOX DRAWINGS HEAVY QUADRUPLE DASH HORIZONTAL
;; U+250A	┊	e2 94 8a	BOX DRAWINGS LIGHT QUADRUPLE DASH VERTICAL
;; U+250B	┋	e2 94 8b	BOX DRAWINGS HEAVY QUADRUPLE DASH VERTICAL

;; U+250C	┌	e2 94 8c	BOX DRAWINGS LIGHT DOWN AND RIGHT
;; U+250D	┍	e2 94 8d	BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
;; U+250E	┎	e2 94 8e	BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
;; U+250F	┏	e2 94 8f	BOX DRAWINGS HEAVY DOWN AND RIGHT
;; U+2510	┐	e2 94 90	BOX DRAWINGS LIGHT DOWN AND LEFT
;; U+2511	┑	e2 94 91	BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
;; U+2512	┒	e2 94 92	BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
;; U+2513	┓	e2 94 93	BOX DRAWINGS HEAVY DOWN AND LEFT
;; U+2514	└	e2 94 94	BOX DRAWINGS LIGHT UP AND RIGHT
;; U+2515	┕	e2 94 95	BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
;; U+2516	┖	e2 94 96	BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
;; U+2517	┗	e2 94 97	BOX DRAWINGS HEAVY UP AND RIGHT
;; U+2518	┘	e2 94 98	BOX DRAWINGS LIGHT UP AND LEFT
;; U+2519	┙	e2 94 99	BOX DRAWINGS UP LIGHT AND LEFT HEAVY
;; U+251A	┚	e2 94 9a	BOX DRAWINGS UP HEAVY AND LEFT LIGHT
;; U+251B	┛	e2 94 9b	BOX DRAWINGS HEAVY UP AND LEFT

;; U+251C	├	e2 94 9c	BOX DRAWINGS LIGHT VERTICAL AND RIGHT
;; U+251D	┝	e2 94 9d	BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
;; U+251E	┞	e2 94 9e	BOX DRAWINGS UP HEAVY AND RIGHT DOWN LIGHT
;; U+251F	┟	e2 94 9f	BOX DRAWINGS DOWN HEAVY AND RIGHT UP LIGHT
;; U+2520	┠	e2 94 a0	BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
;; U+2521	┡	e2 94 a1	BOX DRAWINGS DOWN LIGHT AND RIGHT UP HEAVY
;; U+2522	┢	e2 94 a2	BOX DRAWINGS UP LIGHT AND RIGHT DOWN HEAVY
;; U+2523	┣	e2 94 a3	BOX DRAWINGS HEAVY VERTICAL AND RIGHT

;; U+2524	┤	e2 94 a4	BOX DRAWINGS LIGHT VERTICAL AND LEFT
;; U+2525	┥	e2 94 a5	BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
;; U+2526	┦	e2 94 a6	BOX DRAWINGS UP HEAVY AND LEFT DOWN LIGHT
;; U+2527	┧	e2 94 a7	BOX DRAWINGS DOWN HEAVY AND LEFT UP LIGHT
;; U+2528	┨	e2 94 a8	BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
;; U+2529	┩	e2 94 a9	BOX DRAWINGS DOWN LIGHT AND LEFT UP HEAVY
;; U+252A	┪	e2 94 aa	BOX DRAWINGS UP LIGHT AND LEFT DOWN HEAVY
;; U+252B	┫	e2 94 ab	BOX DRAWINGS HEAVY VERTICAL AND LEFT

;; U+252C	┬	e2 94 ac	BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
;; U+252D	┭	e2 94 ad	BOX DRAWINGS LEFT HEAVY AND RIGHT DOWN LIGHT
;; U+252E	┮	e2 94 ae	BOX DRAWINGS RIGHT HEAVY AND LEFT DOWN LIGHT
;; U+252F	┯	e2 94 af	BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
;; U+2530	┰	e2 94 b0	BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
;; U+2531	┱	e2 94 b1	BOX DRAWINGS RIGHT LIGHT AND LEFT DOWN HEAVY
;; U+2532	┲	e2 94 b2	BOX DRAWINGS LEFT LIGHT AND RIGHT DOWN HEAVY
;; U+2533	┳	e2 94 b3	BOX DRAWINGS HEAVY DOWN AND HORIZONTAL

;; U+2534	┴	e2 94 b4	BOX DRAWINGS LIGHT UP AND HORIZONTAL
;; U+2535	┵	e2 94 b5	BOX DRAWINGS LEFT HEAVY AND RIGHT UP LIGHT
;; U+2536	┶	e2 94 b6	BOX DRAWINGS RIGHT HEAVY AND LEFT UP LIGHT
;; U+2537	┷	e2 94 b7	BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
;; U+2538	┸	e2 94 b8	BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
;; U+2539	┹	e2 94 b9	BOX DRAWINGS RIGHT LIGHT AND LEFT UP HEAVY
;; U+253A	┺	e2 94 ba	BOX DRAWINGS LEFT LIGHT AND RIGHT UP HEAVY
;; U+253B	┻	e2 94 bb	BOX DRAWINGS HEAVY UP AND HORIZONTAL

;; U+253C	┼	e2 94 bc	BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
;; U+253D	┽	e2 94 bd	BOX DRAWINGS LEFT HEAVY AND RIGHT VERTICAL LIGHT
;; U+253E	┾	e2 94 be	BOX DRAWINGS RIGHT HEAVY AND LEFT VERTICAL LIGHT
;; U+253F	┿	e2 94 bf	BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
;; U+2540	╀	e2 95 80	BOX DRAWINGS UP HEAVY AND DOWN HORIZONTAL LIGHT
;; U+2541	╁	e2 95 81	BOX DRAWINGS DOWN HEAVY AND UP HORIZONTAL LIGHT
;; U+2542	╂	e2 95 82	BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
;; U+2543	╃	e2 95 83	BOX DRAWINGS LEFT UP HEAVY AND RIGHT DOWN LIGHT
;; U+2544	╄	e2 95 84	BOX DRAWINGS RIGHT UP HEAVY AND LEFT DOWN LIGHT
;; U+2545	╅	e2 95 85	BOX DRAWINGS LEFT DOWN HEAVY AND RIGHT UP LIGHT
;; U+2546	╆	e2 95 86	BOX DRAWINGS RIGHT DOWN HEAVY AND LEFT UP LIGHT
;; U+2547	╇	e2 95 87	BOX DRAWINGS DOWN LIGHT AND UP HORIZONTAL HEAVY
;; U+2548	╈	e2 95 88	BOX DRAWINGS UP LIGHT AND DOWN HORIZONTAL HEAVY
;; U+2549	╉	e2 95 89	BOX DRAWINGS RIGHT LIGHT AND LEFT VERTICAL HEAVY
;; U+254A	╊	e2 95 8a	BOX DRAWINGS LEFT LIGHT AND RIGHT VERTICAL HEAVY
;; U+254B	╋	e2 95 8b	BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL

;; U+254C	╌	e2 95 8c	BOX DRAWINGS LIGHT DOUBLE DASH HORIZONTAL
;; U+254D	╍	e2 95 8d	BOX DRAWINGS HEAVY DOUBLE DASH HORIZONTAL
;; U+254E	╎	e2 95 8e	BOX DRAWINGS LIGHT DOUBLE DASH VERTICAL
;; U+254F	╏	e2 95 8f	BOX DRAWINGS HEAVY DOUBLE DASH VERTICAL
;; U+2550	═	e2 95 90	BOX DRAWINGS DOUBLE HORIZONTAL
;; U+2551	║	e2 95 91	BOX DRAWINGS DOUBLE VERTICAL

;; U+2552	╒	e2 95 92	BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
;; U+2553	╓	e2 95 93	BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
;; U+2554	╔	e2 95 94	BOX DRAWINGS DOUBLE DOWN AND RIGHT
;; U+2555	╕	e2 95 95	BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
;; U+2556	╖	e2 95 96	BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
;; U+2557	╗	e2 95 97	BOX DRAWINGS DOUBLE DOWN AND LEFT
;; U+2558	╘	e2 95 98	BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
;; U+2559	╙	e2 95 99	BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
;; U+255A	╚	e2 95 9a	BOX DRAWINGS DOUBLE UP AND RIGHT
;; U+255B	╛	e2 95 9b	BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
;; U+255C	╜	e2 95 9c	BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
;; U+255D	╝	e2 95 9d	BOX DRAWINGS DOUBLE UP AND LEFT
;; U+255E	╞	e2 95 9e	BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
;; U+255F	╟	e2 95 9f	BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
;; U+2560	╠	e2 95 a0	BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
;; U+2561	╡	e2 95 a1	BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
;; U+2562	╢	e2 95 a2	BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
;; U+2563	╣	e2 95 a3	BOX DRAWINGS DOUBLE VERTICAL AND LEFT
;; U+2564	╤	e2 95 a4	BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
;; U+2565	╥	e2 95 a5	BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
;; U+2566	╦	e2 95 a6	BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
;; U+2567	╧	e2 95 a7	BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
;; U+2568	╨	e2 95 a8	BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
;; U+2569	╩	e2 95 a9	BOX DRAWINGS DOUBLE UP AND HORIZONTAL
;; U+256A	╪	e2 95 aa	BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
;; U+256B	╫	e2 95 ab	BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
;; U+256C	╬	e2 95 ac	BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL

;; U+256D	╭	e2 95 ad	BOX DRAWINGS LIGHT ARC DOWN AND RIGHT
;; U+256E	╮	e2 95 ae	BOX DRAWINGS LIGHT ARC DOWN AND LEFT
;; U+256F	╯	e2 95 af	BOX DRAWINGS LIGHT ARC UP AND LEFT
;; U+2570	╰	e2 95 b0	BOX DRAWINGS LIGHT ARC UP AND RIGHT

;; U+2571	╱	e2 95 b1	BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
;; U+2572	╲	e2 95 b2	BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
;; U+2573	╳	e2 95 b3	BOX DRAWINGS LIGHT DIAGONAL CROSS
;; U+2574	╴	e2 95 b4	BOX DRAWINGS LIGHT LEFT
;; U+2575	╵	e2 95 b5	BOX DRAWINGS LIGHT UP
;; U+2576	╶	e2 95 b6	BOX DRAWINGS LIGHT RIGHT
;; U+2577	╷	e2 95 b7	BOX DRAWINGS LIGHT DOWN
;; U+2578	╸	e2 95 b8	BOX DRAWINGS HEAVY LEFT
;; U+2579	╹	e2 95 b9	BOX DRAWINGS HEAVY UP
;; U+257A	╺	e2 95 ba	BOX DRAWINGS HEAVY RIGHT
;; U+257B	╻	e2 95 bb	BOX DRAWINGS HEAVY DOWN
;; U+257C	╼	e2 95 bc	BOX DRAWINGS LIGHT LEFT AND HEAVY RIGHT
;; U+257D	╽	e2 95 bd	BOX DRAWINGS LIGHT UP AND HEAVY DOWN
;; U+257E	╾	e2 95 be	BOX DRAWINGS HEAVY LEFT AND LIGHT RIGHT
;; U+257F	╿	e2 95 bf	BOX DRAWINGS HEAVY UP AND LIGHT DOWN
;; U+2580	▀	e2 96 80	UPPER HALF BLOCK
;; U+2581	▁	e2 96 81	LOWER ONE EIGHTH BLOCK
;; U+2582	▂	e2 96 82	LOWER ONE QUARTER BLOCK
;; U+2583	▃	e2 96 83	LOWER THREE EIGHTHS BLOCK
;; U+2584	▄	e2 96 84	LOWER HALF BLOCK
;; U+2585	▅	e2 96 85	LOWER FIVE EIGHTHS BLOCK
;; U+2586	▆	e2 96 86	LOWER THREE QUARTERS BLOCK
;; U+2587	▇	e2 96 87	LOWER SEVEN EIGHTHS BLOCK
;; U+2588	█	e2 96 88	FULL BLOCK
;; U+2589	▉	e2 96 89	LEFT SEVEN EIGHTHS BLOCK
;; U+258A	▊	e2 96 8a	LEFT THREE QUARTERS BLOCK
;; U+258B	▋	e2 96 8b	LEFT FIVE EIGHTHS BLOCK
;; U+258C	▌	e2 96 8c	LEFT HALF BLOCK
;; U+258D	▍	e2 96 8d	LEFT THREE EIGHTHS BLOCK
;; U+258E	▎	e2 96 8e	LEFT ONE QUARTER BLOCK
;; U+258F	▏	e2 96 8f	LEFT ONE EIGHTH BLOCK
;; U+2590	▐	e2 96 90	RIGHT HALF BLOCK

;; U+2591	░	e2 96 91	LIGHT SHADE
;; U+2592	▒	e2 96 92	MEDIUM SHADE
;; U+2593	▓	e2 96 93	DARK SHADE
;; U+2594	▔	e2 96 94	UPPER ONE EIGHTH BLOCK
;; U+2595	▕	e2 96 95	RIGHT ONE EIGHTH BLOCK
;; U+2596	▖	e2 96 96	QUADRANT LOWER LEFT
;; U+2597	▗	e2 96 97	QUADRANT LOWER RIGHT
;; U+2598	▘	e2 96 98	QUADRANT UPPER LEFT
;; U+2599	▙	e2 96 99	QUADRANT UPPER LEFT AND LOWER LEFT AND LOWER RIGHT
;; U+259A	▚	e2 96 9a	QUADRANT UPPER LEFT AND LOWER RIGHT
;; U+259B	▛	e2 96 9b	QUADRANT UPPER LEFT AND UPPER RIGHT AND LOWER LEFT
;; U+259C	▜	e2 96 9c	QUADRANT UPPER LEFT AND UPPER RIGHT AND LOWER RIGHT
;; U+259D	▝	e2 96 9d	QUADRANT UPPER RIGHT
;; U+259E	▞	e2 96 9e	QUADRANT UPPER RIGHT AND LOWER LEFT
;; U+259F	▟	e2 96 9f	QUADRANT UPPER RIGHT AND LOWER LEFT AND LOWER RIGHT
;; U+25A0	■	e2 96 a0	BLACK SQUARE
;; U+25A1	□	e2 96 a1	WHITE SQUARE
;; U+25A2	▢	e2 96 a2	WHITE SQUARE WITH ROUNDED CORNERS
;; U+25A3	▣	e2 96 a3	WHITE SQUARE CONTAINING BLACK SMALL SQUARE
;; U+25A4	▤	e2 96 a4	SQUARE WITH HORIZONTAL FILL
;; U+25A5	▥	e2 96 a5	SQUARE WITH VERTICAL FILL
;; U+25A6	▦	e2 96 a6	SQUARE WITH ORTHOGONAL CROSSHATCH FILL
;; U+25A7	▧	e2 96 a7	SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL
;; U+25A8	▨	e2 96 a8	SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL
;; U+25A9	▩	e2 96 a9	SQUARE WITH DIAGONAL CROSSHATCH FILL
;; U+25AA	▪	e2 96 aa	BLACK SMALL SQUARE
;; U+25AB	▫	e2 96 ab	WHITE SMALL SQUARE
;; U+25AC	▬	e2 96 ac	BLACK RECTANGLE
;; U+25AD	▭	e2 96 ad	WHITE RECTANGLE
;; U+25AE	▮	e2 96 ae	BLACK VERTICAL RECTANGLE
;; U+25AF	▯	e2 96 af	WHITE VERTICAL RECTANGLE
;; U+25B0	▰	e2 96 b0	BLACK PARALLELOGRAM
;; U+25B1	▱	e2 96 b1	WHITE PARALLELOGRAM
;; U+25B2	▲	e2 96 b2	BLACK UP-POINTING TRIANGLE
;; U+25B3	△	e2 96 b3	WHITE UP-POINTING TRIANGLE
;; U+25B4	▴	e2 96 b4	BLACK UP-POINTING SMALL TRIANGLE
;; U+25B5	▵	e2 96 b5	WHITE UP-POINTING SMALL TRIANGLE
;; U+25B6	▶	e2 96 b6	BLACK RIGHT-POINTING TRIANGLE
;; U+25B7	▷	e2 96 b7	WHITE RIGHT-POINTING TRIANGLE
;; U+25B8	▸	e2 96 b8	BLACK RIGHT-POINTING SMALL TRIANGLE
;; U+25B9	▹	e2 96 b9	WHITE RIGHT-POINTING SMALL TRIANGLE
;; U+25BA	►	e2 96 ba	BLACK RIGHT-POINTING POINTER
;; U+25BB	▻	e2 96 bb	WHITE RIGHT-POINTING POINTER
;; U+25BC	▼	e2 96 bc	BLACK DOWN-POINTING TRIANGLE
;; U+25BD	▽	e2 96 bd	WHITE DOWN-POINTING TRIANGLE
;; U+25BE	▾	e2 96 be	BLACK DOWN-POINTING SMALL TRIANGLE
;; U+25BF	▿	e2 96 bf	WHITE DOWN-POINTING SMALL TRIANGLE
;; U+25C0	◀	e2 97 80	BLACK LEFT-POINTING TRIANGLE
;; U+25C1	◁	e2 97 81	WHITE LEFT-POINTING TRIANGLE
;; U+25C2	◂	e2 97 82	BLACK LEFT-POINTING SMALL TRIANGLE
;; U+25C3	◃	e2 97 83	WHITE LEFT-POINTING SMALL TRIANGLE
;; U+25C4	◄	e2 97 84	BLACK LEFT-POINTING POINTER
;; U+25C5	◅	e2 97 85	WHITE LEFT-POINTING POINTER
;; U+25C6	◆	e2 97 86	BLACK DIAMOND
;; U+25C7	◇	e2 97 87	WHITE DIAMOND
;; U+25C8	◈	e2 97 88	WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
;; U+25C9	◉	e2 97 89	FISHEYE
;; U+25CA	◊	e2 97 8a	LOZENGE
;; U+25CB	○	e2 97 8b	WHITE CIRCLE
;; U+25CC	◌	e2 97 8c	DOTTED CIRCLE
;; U+25CD	◍	e2 97 8d	CIRCLE WITH VERTICAL FILL
;; U+25CE	◎	e2 97 8e	BULLSEYE
;; U+25CF	●	e2 97 8f	BLACK CIRCLE
;; U+25D0	◐	e2 97 90	CIRCLE WITH LEFT HALF BLACK
;; U+25D1	◑	e2 97 91	CIRCLE WITH RIGHT HALF BLACK
;; U+25D2	◒	e2 97 92	CIRCLE WITH LOWER HALF BLACK
;; U+25D3	◓	e2 97 93	CIRCLE WITH UPPER HALF BLACK
;; U+25D4	◔	e2 97 94	CIRCLE WITH UPPER RIGHT QUADRANT BLACK
;; U+25D5	◕	e2 97 95	CIRCLE WITH ALL BUT UPPER LEFT QUADRANT BLACK
;; U+25D6	◖	e2 97 96	LEFT HALF BLACK CIRCLE
;; U+25D7	◗	e2 97 97	RIGHT HALF BLACK CIRCLE
;; U+25D8	◘	e2 97 98	INVERSE BULLET
;; U+25D9	◙	e2 97 99	INVERSE WHITE CIRCLE
;; U+25DA	◚	e2 97 9a	UPPER HALF INVERSE WHITE CIRCLE
;; U+25DB	◛	e2 97 9b	LOWER HALF INVERSE WHITE CIRCLE
;; U+25DC	◜	e2 97 9c	UPPER LEFT QUADRANT CIRCULAR ARC
;; U+25DD	◝	e2 97 9d	UPPER RIGHT QUADRANT CIRCULAR ARC
;; U+25DE	◞	e2 97 9e	LOWER RIGHT QUADRANT CIRCULAR ARC
;; U+25DF	◟	e2 97 9f	LOWER LEFT QUADRANT CIRCULAR ARC
;; U+25E0	◠	e2 97 a0	UPPER HALF CIRCLE
;; U+25E1	◡	e2 97 a1	LOWER HALF CIRCLE
;; U+25E2	◢	e2 97 a2	BLACK LOWER RIGHT TRIANGLE
;; U+25E3	◣	e2 97 a3	BLACK LOWER LEFT TRIANGLE
;; U+25E4	◤	e2 97 a4	BLACK UPPER LEFT TRIANGLE
;; U+25E5	◥	e2 97 a5	BLACK UPPER RIGHT TRIANGLE
;; U+25E6	◦	e2 97 a6	WHITE BULLET
;; U+25E7	◧	e2 97 a7	SQUARE WITH LEFT HALF BLACK
;; U+25E8	◨	e2 97 a8	SQUARE WITH RIGHT HALF BLACK
;; U+25E9	◩	e2 97 a9	SQUARE WITH UPPER LEFT DIAGONAL HALF BLACK
;; U+25EA	◪	e2 97 aa	SQUARE WITH LOWER RIGHT DIAGONAL HALF BLACK
;; U+25EB	◫	e2 97 ab	WHITE SQUARE WITH VERTICAL BISECTING LINE
;; U+25EC	◬	e2 97 ac	WHITE UP-POINTING TRIANGLE WITH DOT
;; U+25ED	◭	e2 97 ad	UP-POINTING TRIANGLE WITH LEFT HALF BLACK
;; U+25EE	◮	e2 97 ae	UP-POINTING TRIANGLE WITH RIGHT HALF BLACK
;; U+25EF	◯	e2 97 af	LARGE CIRCLE



(provide '02-mode-084-hydra)

;;; 02-mode-084-hydra.el ends here
