rem Run emacs only when it's not running, otherwise activate it
SETLOCAL EnableExtensions
set EXE=emacs.exe
FOR /F %%x IN ('tasklist /NH /FI "IMAGENAME eq %EXE%"') DO IF %%x == %EXE% goto FOUND

echo Not running
runemacs.exe
goto END

:FOUND
echo Running
nircmd win activate class "Emacs"
nircmd win max class "Emacs"

:END
