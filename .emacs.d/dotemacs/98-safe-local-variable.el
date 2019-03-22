;;; 98-safe-local-variable.el --- a config file about local variable
;;                                .dir-local.el

;; Copyright (c) 2018-2019 Claude Tete
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
;; Created: February 2018
;; Last-Updated: March 2019

;;; Commentary:
;;
;; [HEADER.all customize settings from any .dir-local.el are put in customize]
;; [DEFAULT.t]


;;; Code:

(custom-set-variables
  '(safe-local-variable-values
     (quote
       ((eval progn
          (let
            ((repo-name
               (file-name-nondirectory
                 (directory-file-name
                   (file-name-directory
                     (projectile-project-root)))))
              (user-name
                (getenv "USERNAME")))
            (setenv "VIEWNAME" repo-name)
            (setenv "VIEWPATH"
              (concat "y:/P/" user-name "/" repo-name))
            (setenv "PRODUCTION_DIR"
              (concat "Y:/PROD/" user-name "/" repo-name "/DSS/DebugWithoutCov/dss_fms"))
            (setenv "TEST_UNIT" "False")
            (setenv "CYGWIN" "nodosfilewarning")
            (setenv "VIEWPATH"
              (concat "y:/P/" user-name "/" repo-name))
            (setenv "PRODUCTION_ROOTDIR"
              (concat "Y:\\PROD\\" user-name))
            (setenv "PRODUCTION_DIR"
              (concat "Y:/PROD/" user-name "/" repo-name "/DSS/DebugWithoutCov/dss_fms"))
            (setenv "PROJECT_NAME" "dss_fms")
            (setenv "GNATPROJECT_FILE" "dss_fms.gpr")
            (setenv "GPS_CUSTOM_PATH"
              (concat "y:/P/" user-name "/" repo-name "/build_chain/gps_plugins"))
            (setenv "GPR_PROJECT_PATH"
              (concat "y:/P/" user-name "/" repo-name "/cc_FMS_STUDIO/BUILD/GPS_PROJECT/CONF/"))
            (setenv "PLATFORM" "DSS")
            (setenv "TARGET" "Native")
            (setenv "COMPILATION" "Debug")
            (setenv "INSTRUMENTATION" "WithoutCov")
            (setenv "CLEARAUDIT" "WithoutClearaudit")
            (setenv "GNAT_PATH" "/cygdrive/d/App/GNATPRO_17.1/bin/"))
          (setq tqnr-ada-gps-build-command
            (concat "gprbuild -ws -c -f -u -P"
              (file-name-as-directory
                (projectile-project-root))
              "build_chain/dss/dss_fms.gpr -XInstrumentation=WithoutCov -XCompilation=Debug -XTest_Unit=False -XTarget=Native -XPLATFORM=DSS -XClearaudit=WithoutClearaudit -F"))
          (setq tqnr-ada-gps-check-command
            (concat "gprbuild -q -c -f -gnatc -u -P"
              (file-name-as-directory
                (projectile-project-root))
              "/build_chain/dss/dss_fms.gpr -XInstrumentation=WithoutCov -XCompilation=Debug -XTest_Unit=False -XTarget=Native -XPLATFORM=DSS -XClearaudit=WithoutClearaudit -p -F"))
          (setq tqnr-ada-gps-pretty-print-command
            (concat "gnat pretty -rf -nM -M100 -cl3 -c0 --eol=dos -P"
              (file-name-as-directory
                (projectile-project-root))
              "build_chain/dss/dss_fms.gpr -XInstrumentation=WithoutCov -XCompilation=Debug -XTest_Unit=False -XTarget=Native -XPLATFORM=DSS -XClearaudit=WithoutClearaudit -F"))
          (setq tqnr-ada-gps-build-all-command
            (concat "gprbuild -d -P"
              (file-name-as-directory
                (projectile-project-root))
              "build_chain/dss/dss_fms.gpr -XInstrumentation=WithoutCov -XCompilation=Debug -XTest_Unit=False -XTarget=Native -XPLATFORM=DSS -XClearaudit=WithoutClearaudit Y:\\P\\S0070736\\fms200_product_build\\components\\instance_libs\\DssInitialisation\\code\\factories\\p0_main.adb -j4 -p -largs -Wl,--print-memory-usage -F"))
          (setq tqnr-ada-gps-clean-all-command
            (concat "gprclean -r -P"
              (file-name-as-directory
                (projectile-project-root))
              "build_chain/dss/dss_fms.gpr -XInstrumentation=WithoutCov -XCompilation=Debug -XTest_Unit=False -XTarget=Native -XPLATFORM=DSS -XClearaudit=WithoutClearaudit"))
          (setq tqnr-ada-gps-build-native-command
            (concat "cd "
              (file-name-as-directory
                (projectile-project-root))
              "build_chain/dss && make env dll")))))))


(provide '98-safe-local-variable)

;;; 98-safe-local-variable.el ends here
