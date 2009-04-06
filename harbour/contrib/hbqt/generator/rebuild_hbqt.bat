@echo off
rem $Id$
rem

IF %HB_COMPILER%.==. GOTO ERR_ENV

echo. Making hbqtgen ...
..\..\..\bin\hbmk2.exe hbqtgen
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
hbqtgen.exe qt45.qtp > hbqtgen.log 2>&1
IF NOT ERRORLEVEL 0 GOTO SHOW_ERROR
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
GOTO END_BATCH

:ERR_ENV
echo. Please set correctly your environment:
echo. set HB_COMPILER=mingw
echo. and your PATH, then restart this batch.

:END_BATCH
