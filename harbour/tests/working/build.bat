rem 
rem $Id$
rem 

@echo off
if %hb_architecture%.==. goto bad_arch
if %hb_compiler%.==. goto bad_comp
if exist %hb_architecture\%hb_compiler\%1.* del %hb_architecture\%hb_compiler\%1.*
make -r "PRG_SOURCES=%1.prg"
if not errorlevel 1 %hb_architecture\%hb_compiler\%1 %2 %3 %4 %5 %6 %7 %8 %9
goto exit
:bad_arch
Echo HB_ARCHITECTURE is not set.
goto exit
:bad_comp
Echo HB_COMPILER is not set.
:exit
