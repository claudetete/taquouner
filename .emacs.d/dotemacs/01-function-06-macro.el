;;; 01-function-06-macro.el --- add some function about macro management

;; Copyright (c) 2017-2019 Claude Tete
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
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [SUBHEADER.custom function about macro management]
;; [SUBDEFAULT.t]
;;


;;; Code:

;;; toggle macro recording on/off (by Fabrice Niessen)
(defun toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

;;; toggle macro recording on/off (by Fabrice Niessen)
(defun toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;;; when region is selected call last macro on region else call last macro (by
;;; Claude TETE)
(defun call-last-kbd-macro-region (repeat)
  (interactive "p")
  (if (use-region-p)
    (apply-macro-to-region-lines (region-beginning) (region-end))
    (call-last-kbd-macro repeat)
    )
  )

;; shortcuts are put in a hook to be loaded after everything else in init process
(add-hook 'tqnr-after-init-shortcut-hook
  (lambda ()
    (when (not tqnr-section-mode-hydra-macro)
      ;; macro (by Fabrice Niessen)
      ;; start/stop recording a keyboard macro (if you change it you also must change
      ;; it in functions.el)
      (global-set-key     (kbd "<S-f8>")          'toggle-kbd-macro-recording-on)
      ;; execute the most recent keyboard macro or on each line if a region is
      ;; selected
      (global-set-key     (kbd "<f8>")            'call-last-kbd-macro-region)
      ;; assign a name to the last keyboard macro defined
      (global-set-key     (kbd "<C-f8>")          'name-last-kbd-macro)
      ;; edit the last keyboard macro defined
      (global-set-key     (kbd "<M-f8>")          'edit-last-kbd-macro)
      ;; select previous macro
      (global-set-key     (kbd "<H-f8>")          'kmacro-cycle-ring-previous)
      ;; select next macro
      (global-set-key     (kbd "<H-S-f8>")        'kmacro-cycle-ring-next)
      )
    ) ;; (lambda ()
  ) ;; (add-hook 'tqnr-after-init-shortcut-hook


(provide '01-function-06-macro)

;;; 01-function-06-macro.el ends here
