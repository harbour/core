@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\..\lib\b32\hgfwin32.lib ..\..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\..\lib\b32\hgfwin32.lib del ..\..\..\lib\b32\hgfwin32.lib
   if exist ..\..\..\lib\b32\hgfwin32.bak del ..\..\..\lib\b32\hgfwin32.bak
   if exist ..\..\..\obj\b32\win321.obj   del ..\..\..\obj\b32\win32.obj
   if exist ..\..\..\obj\b32\tform.obj    del ..\..\..\obj\b32\tform.obj
   if exist ..\..\..\obj\b32\tmenu.obj    del ..\..\..\obj\b32\tmenu.obj
   if exist ..\..\..\obj\b32\tmenuitm.obj del ..\..\..\obj\b32\tmenuitm.obj

   goto EXIT

:EXIT

