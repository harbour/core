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

   copy ..\..\lib\b32\hb_btree.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\hb_btree.lib   del ..\..\lib\b32\hb_btree.lib
   if exist ..\..\lib\b32\hb_btree.bak   del ..\..\lib\b32\hb_btree.bak
   if exist ..\..\obj\b32\hb_btree.obj   del ..\..\obj\b32\hb_btree.obj
   if exist ..\..\obj\b32\tbtree.obj     del ..\..\obj\b32\tbtree.obj
   if exist ..\..\obj\b32\tbtree.c       del ..\..\obj\b32\tbtree.c

   goto EXIT

:EXIT

