@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "Clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   nmake /f makefile.vc %1 %2 %3 > make_vc.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\vc\html.lib ..\..\lib\*.* >nul
   goto EXIT

:BUILD_ERR

   notepad make_vc.log

:CLEAN
   if exist ..\..\lib\vc\html.lib      del ..\..\lib\vc\html.lib
   if exist ..\..\lib\vc\html.bak      del ..\..\lib\vc\html.bak
   if exist ..\..\obj\vc\ohtm.obj      del ..\..\obj\vc\ads1.obj
   if exist ..\..\obj\vc\htmbrows.obj  del ..\..\obj\vc\adsfunc.obj
   if exist ..\..\obj\vc\oedit.obj     del ..\..\obj\vc\adsmgmnt.obj
   if exist ..\..\obj\vc\ofile.obj     del ..\..\obj\vc\ofile.obj
   if exist ..\..\obj\vc\jlist.obj     del ..\..\obj\vc\jlist.obj
   if exist ..\..\obj\vc\oini.obj      del ..\..\obj\vc\oini.obj
   if exist ..\..\obj\vc\jwindow.obj   del ..\..\obj\vc\jwindow.obj
   if exist ..\..\obj\vc\ocgi.obj      del ..\..\obj\vc\ocgi.obj
   if exist ..\..\obj\vc\oframe.obj    del ..\..\obj\vc\oframe.obj
   if exist ..\..\obj\vc\counter.obj   del ..\..\obj\vc\counter.obj
   if exist ..\..\obj\vc\errorsys.obj  del ..\..\obj\vc\errorsys.obj
   if exist ..\..\obj\vc\htmutil.obj   del ..\..\obj\vc\htmutil.obj

   goto EXIT

:EXIT

