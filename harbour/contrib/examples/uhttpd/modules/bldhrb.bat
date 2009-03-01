@echo off
rem
rem $Id$
rem

set HB_INSTALL=..\..\..\..
set HB_BIN_INSTALL=%HB_INSTALL%\bin
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
