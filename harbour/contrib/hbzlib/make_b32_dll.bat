@echo off
REM
REM $Id$
REM
REM

if not exist obj\dll md obj\dll
if not exist obj\dll\b32 md obj\dll\b32

:BUILD

   make -fmake_b32_dll.mak %1 %2 %3 > make_b32_dll.log
   if errorlevel 1 goto BUILD_ERR
   if "%1" == "clean" goto CLEAN
   if "%1" == "CLEAN" goto CLEAN
 
:BUILD_OK

if exist hdll.tmp del hdll.tmp
if exist ..\..\lib\b32\hbzipdll.dll implib ..\..\lib\b32\hbzipdll.lib ..\..\lib\b32\hbzipdll.dll > nul
if exist ..\..\lib\b32\hbzipdll.dll copy ..\..\lib\b32\hbzipdll.dll ..\..\lib > nul
if exist ..\..\lib\b32\hbzipdll.lib copy ..\..\lib\b32\hbzipdll.lib ..\..\lib > nul

goto EXIT

:BUILD_ERR

notepad dll_b32.log
goto EXIT

:CLEAN
  if exist dll_b32.log del dll_b32.log

:EXIT
