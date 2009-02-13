@rem
@rem $Id$
@rem

@echo off

if %HB_ARCHITECTURE%.==. goto BAD_ARCH
if %HB_COMPILER%.==. goto BAD_COMP

if exist %HB_ARCHITECTURE%\%HB_COMPILER%\%1.* del %HB_ARCHITECTURE%\%HB_COMPILER%\%1.*
make -r "PRG_SOURCES=%1.prg"
if not errorlevel 1 %HB_ARCHITECTURE%\%HB_COMPILER%\%1 %2 %3 %4 %5 %6 %7 %8 %9
goto EXIT

:BAD_ARCH
echo HB_ARCHITECTURE is not set.
goto EXIT

:BAD_COMP
echo HB_COMPILER is not set.
:EXIT
