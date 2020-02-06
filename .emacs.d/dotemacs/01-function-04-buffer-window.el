;;; 01-function-04-buffer-window.el --- add some function about buffer and window handling

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
;; Version: 0.3
;; Created: July 2017
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about buffer and window handling]
;; [SUBDEFAULT.t]
;;


;;; Code:

;;
;; END OF LINE
;; convert MS-DOS format \r\n to Unix format \n (by ??)
(defun dos2unix ()
  "Transform a DOS file to a Unix file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")
  )
  )

;; convert Unix format \n to MS-DOS format \r\n (by ??)
(defun unix2dos ()
  "Transform a Unix file to a DOS file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")
  )
)

;;
;; SWAP/SPLIT WINDOWS
;; swap 2 windows (by Fabrice Niessen)
(defun my-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
          (message "You need exactly 2 windows to do this."))
    (t
      (let* ((w1 (first (window-list)))
              (w2 (second (window-list)))
              (b1 (window-buffer w1))
              (b2 (window-buffer w2))
              (s1 (window-start w1))
              (s2 (window-start w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)
        )
      )
    )
  )

;; toggle between horizontal and vertical split for 2 windows (by Fabrice
;; Niessen)
(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd (not (and (<= (car this-win-edges)
                                      (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                   (cadr next-win-edges)))))
            (splitter
	      (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
                'split-window-horizontally
		'split-window-vertically)
              )
            )
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1)))
      )
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; swap 2 windows
    (global-set-key     (kbd "C-c ~")   'my-swap-windows)
    ;; toggle the split (horizontal or vertical)
    (global-set-key     (kbd "C-c |")   'my-toggle-window-split)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;;
;; RESIZE WINDOW
;; where is the window vertically by Sergey Ovechkin (pomeo)
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
          (this-window-y-min (nth 1 win-edges))
          (this-window-y-max (nth 3 win-edges))
          (fr-height (frame-height)))
    (cond
      ((eq 0 this-window-y-min) "top")
      ((eq (- fr-height 1) this-window-y-max) "bot")
      ;; TODO do not take into account shackle or popwin window
      (t "mid"))))

;; where is the window horizontally by Sergey Ovechkin (pomeo)
(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
          (this-window-x-min (nth 0 win-edges))
          (this-window-x-max (nth 2 win-edges))
          (fr-width (frame-width)))
    (cond
      ((eq 0 this-window-x-min) "left")
      ((eq (+ fr-width 2) this-window-x-max) "right") ; why 4 ? 2 works for me
      (t "mid"))))

;; what to do when I want to push split line to the top (by Claude TETE)
(defun win-resize-top ()
  (interactive)
  (let ((win-pos (win-resize-top-or-bot)))
    (cond
      ((equal "top" win-pos) (shrink-window 1))
      ((equal "mid" win-pos) (shrink-window 1))
      ((equal "bot" win-pos) (enlarge-window 1))
      )
    )
  )

;; what to do when I want to push split line to the bottom  (by Claude TETE)
(defun win-resize-bottom ()
  (interactive)
  (let ((win-pos (win-resize-top-or-bot)))
    (cond
      ((equal "top" win-pos) (enlarge-window 1))
      ((equal "mid" win-pos) (enlarge-window 1))
      ((equal "bot" win-pos) (shrink-window 1))
      )
    )
  )

;; what to do when I want to push split line to the left (by Claude TETE)
(defun win-resize-left ()
  (interactive)
  (let ((win-pos (win-resize-left-or-right)))
    (cond
      ((equal "left"  win-pos) (shrink-window-horizontally 1))
      ((equal "mid"   win-pos) (shrink-window-horizontally 1))
      ((equal "right" win-pos) (enlarge-window-horizontally 1))
      )
    )
  )

;; what to do when I want to push split line to the right (by Claude TETE)
(defun win-resize-right ()
  (interactive)
  (let ((win-pos (win-resize-left-or-right)))
    (cond
      ((equal "left"  win-pos) (enlarge-window-horizontally 1))
      ((equal "mid"   win-pos) (enlarge-window-horizontally 1))
      ((equal "right" win-pos) (shrink-window-horizontally 1))
      )
    )
  )


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; resize window more easily (before we should use `C-x {'...)
    ;; vertical
    (global-set-key     (kbd "<C-S-up>")        'win-resize-top)
    (global-set-key     (kbd "<C-S-down>")      'win-resize-bottom)
    ;; horizontal
    (global-set-key     (kbd "<C-S-left>")      'win-resize-left)
    (global-set-key     (kbd "<C-S-right>")     'win-resize-right)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;;
;; SWITCH BUFFER
;; Switching Between Two Recently Used Buffers (by Mathias Dahl)
;; like Alt+Tab in Windows Managers (not used)
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  )

;;
;; SCROLL WITH KEEPING CURSOR
;; Scroll the text one line down while keeping the cursor (by Geotechnical
;; Software Services)
(defun scroll-down-keep-cursor ()
  (interactive)
  (scroll-down 1))

;; Scroll the text one line up while keeping the cursor (by Geotechnical
;; Software Services)
(defun scroll-up-keep-cursor ()
  (interactive)
  (scroll-up 1))

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; scroll while keeping cursor position
    (global-set-key     (kbd "<C-S-prior>")          'scroll-up-keep-cursor)
    (global-set-key     (kbd "<C-S-next>")        'scroll-down-keep-cursor)
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


;;
;; MAXIMIZE
;; maximize the current frame (the whole Emacs window) (by Claude TETE)
(defun frame-maximizer ()
  "Maximize the current frame."
  (interactive)
  (when (and tqnr-section-environment-os-recognition tqnr-running-in-graphical)
    (when tqnr-running-on-ms-windows
      (w32-send-sys-command 61488)
      (sit-for 0)
      )
    (when tqnr-running-on-gnu-linux
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
      )
    )
  )

;;
;; SWITCH BUFFER
(defun toggle-special-buffer (buffer)
  "Switch to BUFFER in a special window like ecb compile window."
  (if (not (get-buffer buffer))
    (message (concat "Do not switch, " buffer " does not exist."))
    (let ((buffer-current (buffer-name))
           (window-buffer (get-buffer-window buffer)))
      ;; when buffer is currently displayed in a window (showed)
      (if window-buffer
        ;; when the buffer is the same as the current buffer
        (if (string= buffer-current buffer)
          ;; when ecb is active toggle the compile window
          (if tqnr-section-mode-cedet-ecb
            (ecb-toggle-compile)
            ;; else close current window containing special buffer
            (if tqnr-section-mode-popwin
              ;; when popwin is used
              (popwin:close-popup-window)
              ;; otherwise delete window
              (delete-window)))
          ;; special buffer in a window but not selected so select this window
          (select-window window-buffer))
        ;; special buffer is not in a window (not showed)
        (if tqnr-section-mode-cedet-ecb
          ;; when ecb is toggle ecb compile window
          (ecb-goto-window-compilation)
          (if (and tqnr-section-mode-popwin
                (not (string-match-p (regexp-quote "helm") buffer)))
            ;; show special buffer in popwin
            (popwin:display-buffer buffer)
            ;; show special buffer using built-in switch
            (switch-to-buffer buffer)))
        )
      )
    )
  )

;; go to the grep or ack buffer in special window
(defun toggle-search-buffer ()
  "Switch to the grep or ack buffer."
  (interactive)
  ;; when ack mode and buffer exist
  (if (get-buffer "*ack*")
    (toggle-special-buffer "*ack*")
    (if (get-buffer "*helm-ag*")
      (helm-resume "*helm-ag*")
      (toggle-special-buffer "*grep*")))
  )

;; go to the compilation buffer in special window
(defun toggle-compilation-buffer ()
  "Switch to the compilation buffer."
  (interactive)
  (if (get-buffer "*compilation*")
    (toggle-special-buffer "*compilation*")
    (if (and tqnr-section-function-ada
          (get-buffer ada-gps-buffer-name-last))
      (toggle-special-buffer ada-gps-buffer-name-last)
      (message "Do not switch, no existing compilation buffers."))))

;; go to the vc or vc diff buffer in special window
(defun toggle-vc-buffer ()
  "Switch to the vc or vc diff buffer."
  (interactive)
  (let ((magit-status-buffer (if tqnr-section-mode-projectile
                               (concat "magit: " (projectile-project-name))
                               "")))
    (message (concat "=" magit-status-buffer "="))
    (if (get-buffer magit-status-buffer)
      (toggle-special-buffer magit-status-buffer)
      ;; when vc diff buffer already exist
      (if (get-buffer "*vc-diff*")
        (toggle-special-buffer "*vc-diff*")
        (toggle-special-buffer "*vc*"))
      )
    )
  )

;; go to the occur buffer in special window
(defun toggle-occur-buffer ()
  "Switch to the occur buffer."
  (interactive)
  (toggle-special-buffer "*Occur*")
  )

;; go to the help buffer in special window
(defun toggle-help-buffer ()
  "Switch to the help buffer."
  (interactive)
  ;; TODO add helpful
  (toggle-special-buffer "*Help*")
  )

;; go to the help buffer in special window
(defun toggle-bookmark-buffer ()
  "Switch to the bookmark buffer."
  (interactive)
  (if (get-buffer "*Bookmark List*")
    (toggle-special-buffer "*Bookmark List*")
    (bookmark-bmenu-list))
  )

;; go to the help buffer in special window
(defun toggle-tag-buffer ()
  "Switch to the tag buffer."
  (interactive)
  (if (get-buffer "*helm gtags*")
    (helm-resume "*helm gtags*")))

(defun toggle-file-buffer ()
  "Switch to last file buffer."
  (interactive)
  (when helm-buffers
    (let ((candidates
            (remove-if
              #'(lambda (buffer)
                  (not (member buffer '("*helm find files*"
                                        "*helm buffers*"
                                        "*helm projectile*"))))
              helm-buffers))
           (buffer))
      (setq buffer (helm :sources (helm-build-sync-source "Resume helm buffer"
                                    :candidates candidates)
                     :resume 'noresume
                     :buffer "*helm resume*"))
      (helm-resume buffer)))
  ;; FIXME resume last helm buffer matching a list name
  )

;; go to the symbol reference buffer in special window
(defun toggle-symref-buffer ()
  "Switch to the Symref buffer."
  (interactive)
  (toggle-special-buffer "*Sy*")
  )


;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    ;; switch to tag buffer
    (global-set-key     (kbd "M-2")     'toggle-tag-buffer)
    ;; switch to file buffer
    (global-set-key     (kbd "M-@")   'toggle-file-buffer)
    ;; switch to grep or ack buffer
    (global-set-key     (kbd "M-3")     'toggle-search-buffer)
    ;; switch to compile buffer
    (global-set-key     (kbd "M-4")     'toggle-compilation-buffer)
    ;; switch to vc buffer
    (global-set-key     (kbd "M-5")     'toggle-vc-buffer)
    ;; switch to help buffer
    (global-set-key     (kbd "M-6")     'toggle-help-buffer)

    ;; the previous global-set-key are unset in diff mode ???
    (add-hook 'diff-mode-hook
      (lambda ()
        ;; switch to tag buffer
        (local-set-key  (kbd "M-2")     'toggle-tag-buffer)
        ;; switch to file buffer
        (local-set-key  (kbd "M-S-2")   'toggle-file-buffer)
        ;; switch to grep or ack buffer
        (local-set-key  (kbd "M-3")     'toggle-search-buffer)
        ;; switch to compile buffer
        (local-set-key  (kbd "M-4")     'toggle-compilation-buffer)
        ;; switch to vc buffer
        (local-set-key  (kbd "M-5")     'toggle-vc-buffer)
        ;; switch to help buffer
        (local-set-key  (kbd "M-6")     'toggle-help-buffer)))
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook

(when (and tqnr-section-mode-popwin tqnr-section-mode-helm)
  (defvar tqnr-last-popup-type nil
    "Last type of popup window helm vs popwin")
  (defun tqnr-set-last-popup ()
    "Check when pop popwin or helm and store type."
    (if (popwin:popup-window-live-p)
      (setq tqnr-last-popup-type "popwin")
      (setq tqnr-last-popup-type "helm")))

  ;; when popwin buffer/window is created and showed
  (add-hook 'popwin:after-popup-hook #'tqnr-set-last-popup)
  ;; when helm is not already created
  (add-hook 'helm-before-initialize-hook #'tqnr-set-last-popup)

  (defun resume-popwin-or-helm ()
    "Will resume popwin or helm regarding last closed or close current popwin
session."
    (interactive)
    ;; is popwin running ?
    (if (popwin:popup-window-live-p)
      ;; close current popwin
      (popwin:close-popup-window)
      ;; is last opened was popwin or helm
      (if (string-equal tqnr-last-popup-type "popwin")
        ;; resume popwin
        (popwin:popup-last-buffer)
        ;; resume helm
        (helm-resume helm-last-buffer))))

  (defun resume-helm-choice ()
    "Will resume helm by asking which buffer to resume."
    (interactive)
    ;; resume helm
    (helm-resume t))


  ;; shortcuts are put in a hook to be loaded after everything else in init process
  (add-hook 'tqnr-after-init-shortcut-hook
    (lambda ()
      ;; F2 resume last session of popwin or helm (F2 is already set in helm map to quit)
      (global-set-key   (kbd "<f2>")    #'resume-popwin-or-helm)
      ;; M-F2
      (global-set-key   (kbd "<M-f2>")  #'resume-helm-choice)
      ) ;; (lambda ()
    ) ;; (add-hook 'tqnr-after-init-shortcut-hook
  )


(provide '01-function-04-buffer-window)

;;; 01-function-04-buffer-window.el ends here
