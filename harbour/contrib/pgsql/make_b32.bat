@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\libhbpg.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist ..\..\lib\b32\libhbpg.lib    del ..\..\lib\b32\libhbpg.lib
   if exist ..\..\lib\b32\libhbpg.bak    del ..\..\lib\b32\libhbpg.bak
   if exist ..\..\obj\b32\postgres.obj   del ..\..\obj\b32\postgres.obj
   if exist ..\..\obj\b32\TPostgres.c    del ..\..\obj\b32\TPostgres.c
   if exist ..\..\obj\b32\TPostgres.obj  del ..\..\obj\b32\TPostgres.obj
   goto EXIT

:EXIT
