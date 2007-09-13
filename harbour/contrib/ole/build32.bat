@echo off

SET HB_INCLUDE=..\..\include

bcc32 -c -I.;%HB_INCLUDE% ole2.c
if errorlevel 1 goto end

harbour oleauto /n /i%HB_INCLUDE%
if errorlevel 1 goto end

bcc32 -M -c -O2 -I%HB_INCLUDE% -tW oleauto.c
if errorlevel 1 goto end

if exist hbole.lib del hbole.lib
tlib hbole +ole2 +oleauto
if errorlevel 1 goto end

copy hbole.lib ..\..\lib\*.* > nul

:END
del oleauto.c
del ole2.obj
del oleauto.obj
del hbole.lib
