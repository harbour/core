@echo off
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD
   if not exist obj\b32\dll md obj\b32\dll
   if not exist obj\b32\dll\ct md obj\b32\dll\ct

rem ---------------------------------------------------------------
rem Set any of these LINK_* to 'NO' if you don't want to have
rem appropriate modules in the harbour.dll
rem ---------------------------------------------------------------
   SET LINK_ADS=YES
   SET LINK_CT=YES
   SET LINK_LIBMISC=YES
   SET LINK_DEBUG=YES

   make -fhrbdll.bc %1 %2 %3 > makedll.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy lib\b32\harbour.dll lib\harbour.dll
   implib lib\harbour.lib lib\harbour.dll
   goto EXIT

:BUILD_ERR

   notepad makedll.log
   goto EXIT

:CLEAN

   if exist obj\b32\dll\*.c del obj\b32\dll\*.c
   if exist obj\b32\dll\*.obj del obj\b32\dll\*.obj
   if exist obj\b32\dll\*.h del obj\b32\dll\*.h
   if exist obj\b32\dll\ct\*.c del obj\b32\dll\ct\*.c
   if exist obj\b32\dll\ct\*.obj del obj\b32\dll\ct\*.obj
   if exist lib\b32\harbour.dll del lib\b32\harbour.dll
   if exist lib\b32\*.tds del lib\b32\*.tds
   if exist lib\b32\*.map del lib\b32\*.map
   if exist makedll.log  del makedll.log
   goto EXIT

:EXIT

