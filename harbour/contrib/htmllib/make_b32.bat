@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "Clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\html.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\html.lib      del ..\..\lib\b32\html.lib
   if exist ..\..\lib\b32\html.bak      del ..\..\lib\b32\html.bak
   if exist ..\..\obj\b32\ohtm.obj      del ..\..\obj\b32\ads1.obj
   if exist ..\..\obj\b32\htmbrows.obj  del ..\..\obj\b32\adsfunc.obj
   if exist ..\..\obj\b32\oedit.obj     del ..\..\obj\b32\adsmgmnt.obj
   if exist ..\..\obj\b32\ofile.obj     del ..\..\obj\b32\ofile.obj
   if exist ..\..\obj\b32\jlist.obj     del ..\..\obj\b32\jlist.obj
   if exist ..\..\obj\b32\oini.obj      del ..\..\obj\b32\oini.obj
   if exist ..\..\obj\b32\jwindow.obj   del ..\..\obj\b32\jwindow.obj
   if exist ..\..\obj\b32\ocgi.obj      del ..\..\obj\b32\ocgi.obj
   if exist ..\..\obj\b32\oframe.obj    del ..\..\obj\b32\oframe.obj
   if exist ..\..\obj\b32\counter.obj   del ..\..\obj\b32\counter.obj
   if exist ..\..\obj\b32\errorsys.obj  del ..\..\obj\b32\errorsys.obj
   if exist ..\..\obj\b32\htmutil.obj   del ..\..\obj\b32\htmutil.obj

   goto EXIT

:EXIT

