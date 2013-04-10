;; generic project
(ede-cpp-root-project "myproject"
  :name "MyProject" ; see at top of ecb directory
  :file "c:/path/to/myproject.ede.el/GTAGS" ; create the GTAGS file if not exist
  :include-path '(
                   "./include"
                   "./other/include" ; put all the include path from your project
                   )
  :system-include-path '(
                          "c:/path/to/compiler/include"
                          "c:/path/to/compiler/other/include" ; put all include path from not your project
                          )
  :spp-table '(
                ("FLAG1"    . "")
                ("FLAG2"  . "myflag") ; define here each macro/flag are defined when you build your project
                )
  :local-variables (list
                     (cons 'compile-command "cd c:/path/to/makefile && run your compilation") ; define here the command line to run the compilation for this project (F10 by default)
                     )
  )
