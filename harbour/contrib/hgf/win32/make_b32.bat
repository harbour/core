@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------

rem Save the user value, force silent file overwrite with COPY
rem (not all Windows versions support the COPY /Y flag)
set HB_ORGENV_COPYCMD=%COPYCMD%
set COPYCMD=/Y

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmake_b32.mak %1 %2 %3 > make_b32.log
   if errorlevel 1 notepad make_b32.log

:BUILD_OK

   copy ..\..\..\lib\b32\hgfwin32.lib ..\..\..\lib\*.* > nul
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

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%
