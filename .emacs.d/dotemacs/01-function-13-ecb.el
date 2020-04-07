;;; 01-function-13-ecb.el --- add some function to ecb mode -*- lexical-binding: t -*-

;; Copyright (c) 2012-2019 Claude Tete
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
;; Version: 1.5
;; Created: June 2012
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about ecb mode]
;; [SUBDEFAULT.nil]


;;; Code:

(defvar ecb-toggle-compile-hide-hook nil
  "Run after hide compile window.")

;; internal variables
(defvar ecb-hidden-before nil)
(defvar ecb-maximized-compile-window-state nil)

;; increase/decrease width of ecb window (by Claude TETE)
;; enlarge and shrink window can also be used
(defun ecb-toggle-width ()
  "Toggle variable ecb-window-width between 10% and 25%."
  (interactive)
  ;; toggle between 0.1 and 0.25 of width
  (set-variable 'ecb-windows-width (if (= ecb-windows-width 0.1) 0.25 0.1))
  ;; redraw ecb window
  (ecb-redraw-layout)
  (message "Ecb width set to %d." ecb-windows-width)
  )

;; open ecb history when ecb window is hidden (by Claude TETE)
(defun ecb-open-history ()
  "Open ECB history even when ECB window is hidden."
  (interactive)
  ;; get state of compile window
  (setq ecb-maximized-compile-window-state (ecb-compile-window-state))
  ;;
  ;; when ECB window is hidden or already maximized
  (if (not (ecb-windows-all-displayed))
    (progn
      ;; show ECB window
      (ecb-toggle-ecb-windows 1)
      ;; redraw ecb window
      (ecb-redraw-layout)
      ;; go to history window
      (ecb-goto-window-history)
      ;; keep status of opened from hidden window
      (setq ecb-hidden-before t)
      )
    ;; go to history window
    (ecb-goto-window-history)
    )
  ;;
  ;; fix a bug about the compile window show
  ;; when the compil window was hidden
  (when (not (equal 'visible ecb-maximized-compile-window-state))
    ;; hide compile window
    (ecb-toggle-compile-window -1)
    )
  )

;; open ecb tree directory when ecb window is hide (by Claude TETE)
(defun ecb-open-directories ()
  "Open ECB Directories even when ECB window is hidden."
  (interactive)
  ;; get state of compile window
  (setq ecb-maximized-compile-window-state (ecb-compile-window-state))
  ;; when ECB window is hidden
  (if (not (ecb-windows-all-displayed))
    (progn
      ;; show ECB window
      (ecb-toggle-ecb-windows 1)
      ;; redraw ecb window
      (ecb-redraw-layout)
      ;; go to directory window
      (ecb-goto-window-directories)
      ;; keep status of opened from hidden window
      (setq ecb-hidden-before t)
      )
    ;; go to directory window
    (ecb-goto-window-directories)
    )
  ;;
  ;; fix a bug about the compile window
  ;; when the compil window was hidden
  (when (not (equal 'visible ecb-maximized-compile-window-state))
    ;; hide compile window
    (ecb-toggle-compile-window -1)
    )
  )

;; open ecb source list when ecb window is hide (by Claude TETE)
(defun ecb-open-sources ()
  "Open ECB Sources even when ECB window is hidden."
  (interactive)
  ;; get state of compile window
  (setq ecb-maximized-compile-window-state (ecb-compile-window-state))
  ;; when ECB window is hidden
  (if (not (ecb-windows-all-displayed))
    (progn
      ;; show ECB window
      (ecb-toggle-ecb-windows 1)
      ;; redraw ecb window
      (ecb-redraw-layout)
      ;; go to sources window
      (ecb-goto-window-sources)
      ;; keep status of opened from hidden window
      (setq ecb-hidden-before t)
      )
    ;; go to sources window
    (ecb-goto-window-sources)
    )
  ;;
  ;; fix a bug about the compile window
  ;; when the compil window was hidden
  (when (not (equal 'visible ecb-maximized-compile-window-state))
    ;; hide compile window
    (ecb-toggle-compile-window -1)
    )
  )

;; open ecb function list when ecb window is hide (by Claude TETE)
(defun ecb-open-methods ()
  "Open ECB Methods even when ECb window is hidden."
  (interactive)
  ;; get state of compile window
  (setq ecb-maximized-compile-window-state (ecb-compile-window-state))
  ;; when ECB window is hidden
  (if (not (ecb-windows-all-displayed))
    (progn
      ;; show ECB window
      (ecb-toggle-ecb-windows 1)
      ;; redraw ecb window
      (ecb-redraw-layout)
      ;; go to methods window
      (ecb-goto-window-methods)
      ;; keep status of opened from hidden window
      (setq ecb-hidden-before t)
      )
    ;; go to methods window
    (ecb-goto-window-methods)
    )
  ;;
  ;; fix a bug about the compile window
  ;; when the compil window was hidden
  (when (not (equal 'visible ecb-maximized-compile-window-state))
    ;; hide compile window
    (ecb-toggle-compile-window -1)
    )
  )

;; open a file and hide window if it is maximized (by Claude TETE)
(defun ecb-select ()
  "Select at point and hide if it was hidden."
  (interactive)
  ;; only when the ecb window was hidden before maximize
  (if ecb-hidden-before
    (progn
      ;; do same action than enter
      (tree-buffer-select 0 nil)
      ;; hide ecb window
      (ecb-toggle-ecb-windows -1)
      )
    (progn
      ;; do same action than enter
      (tree-buffer-select 0 nil)
      )
    )
  )

;; open a file and hide window if it is maximized (by Claude TETE)
(defun ecb-directories-select ()
  "Select at point and point to source."
  (interactive)
  ;; do same action than enter
  (tree-buffer-select 0 nil)
  ;; go to sources window
  (ecb-goto-window-sources)
  )

;; open a file and hide window if it is maximized (by Claude TETE)
(defun ecb-sources-select ()
  "Select at point and point to method."
  (interactive)
  ;; do same action than enter
  (tree-buffer-select 0 nil)
  ;; go to methods window
  (ecb-goto-window-methods)
  )

;; hide window if it is maximized (by Claude TETE)
(defun ecb-cancel ()
  "Return to last edit and hide if it was hidden."
  (interactive)
  ;; go to last edit
  (ecb-goto-window-edit-last)
  ;; when it was hidden before
  (when ecb-hidden-before
    ;; hide ecb window
    (ecb-toggle-ecb-windows -1)
    )
  )

;; (un)maximize ecb window
(defun ecb-toggle-maximize ()
  "Toggle maximizing window."
  (interactive)
  ;; when window is maximized
  (if (ecb-buffer-is-maximized-p)
    (progn
      ;; get buffer name
      (let ((buf-name (buffer-name)))
        ;; unmaximized
        (ecb-toggle-maximize-ecb-window)
        ;; for safety check buffer is an ecb buffer
        (when (ecb-buffer-is-ecb-buffer-of-current-layout-p buf-name)
          ;; go to the ecb window
          ;; because after unmaximize it goes to last edit
          (ecb-goto-ecb-window buf-name)
          )
        )
      )
    ;; maximized
    (ecb-toggle-maximize-ecb-window)
    )
  )

;; Show/Hide compile window
(defun ecb-toggle-compile ()
  "Toggle compile window."
  (interactive)
  ;; when compile window is hidden
  (if (not (equal 'visible (ecb-compile-window-state)))
    (progn
      ;; show
      (ecb-toggle-compile-window 1)
      ;; go to
      (ecb-goto-window-compilation))
    (progn
      ;; hide
      (ecb-toggle-compile-window -1)
      (run-hooks 'ecb-toggle-compile-hide-hook))))

;;;; Show/Hide compile window for completions in minibuffer
;;(defun my-display-completions (buf)
;;  "put the *completions* buffer in the right spot"
;;  (let (ret)
;;    (let ((windows (delete (minibuffer-window) (window-list))))
;;      (if (not (equal 'visible (ecb-compile-window-state)))
;;        (progn
;;          (ecb-toggle-compile-window 1)
;;          (ecb-goto-window-compilation)
;;          )
;;        (ecb-goto-window-compilation)
;;        )
;;      (let ((target-window (selected-window)))
;;        (set-window-buffer target-window buf)
;;        target-window
;;        )
;;      )
;;    )
;;  )


(provide '01-function-13-ecb)

;;; 01-function-13-ecb.el ends here
