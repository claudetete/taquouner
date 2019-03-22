@SETLOCAL
@SET "client=D:\Users\ctete\tools\emacs-24.2\bin\emacsclientw.exe"

@SET "editor=D:\Users\ctete\tools\emacs-24.2\bin\runemacs.exe" 

@SET "mine=%1"
@CALL SET mine=%mine:\=/%

@SET "base=%2"
@CALL SET base=%base:\=/%

@START %client% -a "%editor%" -n -c -e "(ediff-files \"%mine%\" \"%base%\")"
