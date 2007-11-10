@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmake_b32.mak %1 %2 %3 > make_b32.log
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
   if exist ..\..\..\obj\b32\win32.obj    del ..\..\..\obj\b32\win32.obj
   if exist ..\..\..\obj\b32\button.obj   del ..\..\..\obj\b32\button.obj
   if exist ..\..\..\obj\b32\edit.obj     del ..\..\..\obj\b32\edit.obj
   if exist ..\..\..\obj\b32\form.obj     del ..\..\..\obj\b32\form.obj
   if exist ..\..\..\obj\b32\menu.obj     del ..\..\..\obj\b32\menu.obj
   if exist ..\..\..\obj\b32\menuitm.obj  del ..\..\..\obj\b32\menuitm.obj
   if exist ..\..\..\obj\b32\winctrl.obj  del ..\..\..\obj\b32\winctrl.obj

   goto EXIT

:EXIT

