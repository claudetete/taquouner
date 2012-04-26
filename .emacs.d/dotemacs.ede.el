;; generic project
(ede-cpp-root-project "dotemacs"
  :name "DotEmacs"
  :file "c:/path/to/myproject.ede.el/GTAGS" ; create the GTAGS file if not exist
  :include-path '(
                   "./plugins" ; put all the include path from your project
                   )
  )
