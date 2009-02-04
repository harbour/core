@echo off
rem
rem $Id$
rem

rem Saving current HB_MT state
set OLDENVMT=%HB_MT%
set OLDENVGT=%HB_GT_LIB%
set OLDENVC=%CFLAGS%
set OLDENVHB=%HARBOURFLAGS%
set OLD_HB_ARCHITECTURE=%HB_ARCHITECTURE%
set OLD_HB_COMPILER=%HB_COMPILER%
set OLD_HB_USER_LIBS=%HB_USER_LIBS%

set HB_INSTALL=..\..\..\..
if %HB_ARCHITECTURE%.==. set HB_ARCHITECTURE=win
if %HB_COMPILER%.==.     set HB_COMPILER=bcc32
SET HB_BIN_INSTALL=%HB_INSTALL%\bin
set HB_INC_INSTALL=include;%HB_INSTALL%\include
set HB_LIB_INSTALL=%HB_INSTALL%\lib

%HB_BIN_INSTALL%\harbour %1.prg -n -q0 -w3 -es2 -gh -i%HB_INC_INSTALL% %2 %3 %HARBOURFLAGS% > bldtest.log

IF ERRORLEVEL 1 GOTO SHOWERROR

GOTO COMPILEOK

:SHOWERROR
echo.
echo.Error on compiling ...
echo.
echo.Running Notepad, please close to end this batch file ...
echo.
notepad bldtest.log
echo.
echo.Notepad closed, exiting ...
echo.
GOTO ENDSET

:COMPILEOK
echo.
echo.Compiled successfully
echo.
if exist bldtest.log del bldtest.log
if exist %1.hrb copy %1.hrb ..\home\cgi-bin /y
if exist %1.hrb del %1.hrb
GOTO ENDSET

:ENDSET
rem Restore Old Settings
set HB_MT=%OLDENVMT%
set HB_GT_LIB=%OLDENVGT%
set CFLAGS=%OLDENVC%
set HARBOURFLAGS=%OLDENVHB%
set HB_ARCHITECTURE=%OLD_HB_ARCHITECTURE%
set HB_COMPILER=%OLD_HB_COMPILER%
set HB_USER_LIBS=%OLD_HB_USER_LIBS%

set OLDENVHB=
set OLDENVGT=
set OLDENVC=
set OLDENVMT=
set BLDDEFAULT=
set OLD_HB_ARCHITECTURE=
set OLD_HB_COMPILER=
set OLD_HB_USER_LIBS=
