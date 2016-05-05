rem Run emacs only when it's not running, otherwise activate it
SETLOCAL EnableExtensions
set EXE=emacs.exe
FOR /F %%x IN ('tasklist /NH /FI "IMAGENAME eq %EXE%"') DO IF %%x == %EXE% goto FOUND

echo Not running
runemacs.exe
goto END

:FOUND
echo Running
@rem nircmd win activate class "Emacs"
@rem nircmd win max class "Emacs"
autoit3 /AutoIt3ExecuteLine WinActivate('[CLASS:Emacs]')

:END
