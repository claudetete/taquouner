;; generic project
(ede-cpp-root-project "dotemacs"
  :name "DotEmacs"
  :file "c:/path/to/myproject.ede.el/GTAGS" ; create the GTAGS file if not exist
  :include-path '(
                   "./plugins" ; put all the include path from your project
                   )
  :spp-table '(
                ("__MY_DEFINE__"       . "") ; put all macro defined by compiling
                ("__MY_SECOND_DEFINE"  . "its_value")
                )
  :local-variables (list
                     (cons 'compile-command "cd /to/my/project && the command line to compile my project") ; put the compilation command
                     (cons 'c-macro-preprocessor "the command line to preprocess") ; only if you want run preprocessing on selected region
                     (cons 'c-macro-cppflags "flags/macro/define to preprocess")
                     )
  )
