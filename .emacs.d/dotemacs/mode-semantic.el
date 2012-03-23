;;; mode-semantic.el --- a config file for semantic mode settings

;; Copyright (c) 2010, 2011, 2012 Claude Tete
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

;; Keywords: config, semantic, bovinate, cedet
;; Author: Claude Tete  <claude.tete@gmail.com>
;; Version: 1.2
;; Created: August 2010
;; Last-Updated: March 2012

;;; Commentary:
;;
;; load by `mode.el' (all requirements are defined in `emacs.el')
;; REQUIREMENT: var     `section-mode-cedet-semantic'
;;              var     `section-mode-cedet'
;;
;; cedet included in Emacs 23 is not the same than cedet-snapshot so it need
;; different parameter
;;
;; To help identify speed issues with the semantic analyzer, you can use
;; the `semantic-elp.el' tool.  To use it type:
;;
;;      M-x semantic-elp-analyze RET
;;
;; It will do a full run for each stage of analysis keeping profiling
;; information.  The analysis will be done at point, so be sure to
;; position the cursor at the location where the slowdown occurs.
;;
;; Once done, it will ask you to save the profiler information.  It will
;; then show you the profile information in ADEBUG mode, so you can
;; navigate through the results.
;;
;; Use `SPC' to open sections of the tree.  Use `SPC' on the sort option
;; to change sorting of the results table.
;;
;; Use `M-x semantic-elp-load-old-run' to restore an old run and view it
;; again.

;;; Change Log:
;; 2012-03-20 (1.2)
;;    add settings + use gnu global to locate file
;; 2012-03-02 (1.1)
;;    split in two settings for emacs included cedet and cedet from repository
;;    (bzr)
;; 2011-08-10 (1.0)
;;    add gnu global to semantic
;; 2011-07-25 (0.1)
;;    creation from scratch (no history since)


;;; Code:
;;
;;; CEDET bzr
(if clt-cedet-bzr
  (progn
    ;; set option to locate a file
    ;;(setq ede-locate-setup-options '(ede-locate-global ede-locate-base))

    ;; launch ede to have the file project management
    (global-ede-mode t)

    ;; Enabling Semantic (code-parsing, smart completion) features
    ;; Select one of the following:

    ;;;; * This enables the database and idle reparse engines
    ;;(semantic-load-enable-minimum-features)

    ;;;; * This enables some tools useful for coding, such as summary mode
    ;;;;   imenu support, and the semantic navigator
    ;;(semantic-load-enable-code-helpers)

    ;;;; * This enables even more coding tools such as intellisense mode
    ;;;;   decoration mode, and stickyfunc mode (plus regular code helpers)
    ;;(semantic-load-enable-gaudy-code-helpers)

      (semantic-load-enable-excessive-code-helpers)

    ;; additional features for names completion, and displaying of information
    ;; for tags & classes
    ;; package.
    (require 'semantic-ia)

    ;; load the different projects
    (load-file (concat dotemacs-path "/dotemacs/project.el"))

    ;;;; To add some path to list of system include paths
    ;;(semantic-add-system-include "" 'c-mode)

    ;;;; Enable SRecode (Template management) minor-mode.
    ;;(global-srecode-minor-mode 1)

    ;;;; current parse state of the current buffer is displayed in the mode line
    ;;(semantic-show-parser-state-mode t) ; do nothing ?

    ;;;; the header line is enabled, and the first line of the current function
    ;;;; or method is displayed in it.
    ;;(semantic-stickyfunc-mode t) ; already set with (semantic-load-enable-excessive-code-helpers)

    ;; need to try again
    ;;;; Increase the delay before activation
    ;;(setq semantic-idle-scheduler-idle-time 10)
    ;;;; Don't reparse really big buffers.
    ;;(setq semantic-idle-scheduler-max-buffer-size 100000)
    ;;;; Increase the delay before doing slow work to 2 minutes.
    ;;(setq semantic-idle-scheduler-work-idle-time 120)

    ;;;; active semanticdb
    ;;(require 'semanticdb) ; already done with (semantic-load-enable-excessive-code-helpers)
    ;;(global-semanticdb-minor-mode 1)

    ;; set option to locate a file with GNU Global
    (custom-set-variables
      '(ede-locate-setup-options (quote (ede-locate-global ede-locate-locate)))
      )
    ) ; (progn

;;
;;; CEDET included in emacs
  (progn
    ;; launch semantic mode
    (semantic-mode t)

    ;; lance ede pour avoir la gestion des fichiers de projet
    (global-ede-mode t)

    ;;;; set GNU/Global as fisrt database
    ;;(setq ede-locate-setup-options '(ede-locate-global ede-locate-base)) ; do not work ?

    ;; load the different projects
    (load-file (concat dotemacs-path "/dotemacs/project.el"))

    ;; semanticdb is used
    (try-require 'semantic/db)

    ;;;; load GNU/Global for semantic
    ;;(try-require 'semantic/db-global)
    ;;;; sematic database is used in c and c++ mode
    ;;(semanticdb-enable-gnu-global-databases 'c-mode)
    ;;(semanticdb-enable-gnu-global-databases 'c++-mode)

    (custom-set-variables

      ;; SEMANTIC SETTING
      ;; choose the semanticdb at submode
      '(semantic-default-submodes
         '(
            global-semantic-highlight-func-mode
            global-semantic-decoration-mode
            global-semantic-stickyfunc-mode
            global-semantic-idle-scheduler-mode
            global-semanticdb-minor-mode))

      ;; do not use image with semantic
      '(semantic-format-use-images-flag nil)

      ;; set the path of the database
      '(semanticdb-default-save-directory (concat dotemacs-path "/semanticdb"))

      '(global-semanticdb-minor-mode t)

      ;; show the function name in the header line
      '(global-semantic-stickyfunc-mode t)
      )

    ;; Increase the delay before activation
    (setq semantic-idle-scheduler-idle-time 10)
    ;; Don't reparse really big buffers.
    (setq semantic-idle-scheduler-max-buffer-size 100000)

    ;; Increase the delay before doing slow work to 2 minutes.
    (setq semantic-idle-scheduler-work-idle-time 120)

    ;; Minor mode for displaying parser cache state in the modeline.
    (setq semantic-show-parser-state-mode t)
    ) ; (progn
  ) ; (if clt-cedet-bzr

;;; mode-semantic.el ends here
