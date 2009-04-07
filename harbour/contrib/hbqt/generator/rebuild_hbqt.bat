@rem
@rem $Id$
@rem

@echo off

echo. Making hbqtgen ...
set _HBMK=hbmk2.exe
if exist ..\..\..\bin\hbmk2.exe set _HBMK=..\..\..\bin\hbmk2.exe
%_HBMK% hbqtgen
IF ERRORLEVEL 1 GOTO SHOW_ERROR
echo. done
echo.
echo. Cleaning gensource and doc folders ...
del ..\gensource\*.prg > nul
del ..\gensource\*.cpp > nul
del ..\gensource\*.h   > nul
del ..\gensource\Makefile_gen > nul
del ..\doc\*.txt > nul
echo. done
echo.
echo. Generating gensource and doc files
hbqtgen.exe > hbqtgen.log 2>&1
IF ERRORLEVEL 1 GOTO SHOW_ERROR
echo. done
echo.
echo. Cleaning hbqt root files ...
del ..\*.prg > nul
del ..\*.cpp > nul
del ..\*.h   > nul
del ..\Makefile_gen. > nul
echo. done
echo.
echo. Copying source files ...
copy ..\gensource\*.prg .. > nul
copy ..\gensource\*.cpp .. > nul
copy ..\gensource\*.h ..   > nul
copy ..\gensource\Makefile_gen .. > nul
echo. done
echo.
echo. Change folder to hbqt root
cd ..
echo. completed.
echo.
echo. Please now rebuild SVN using command:
echo. make_gnu install
echo.

GOTO END_BATCH

:SHOW_ERROR
echo. There is an error on building hbqt.
echo. Please check hbqtgen.log file

:END_BATCH
