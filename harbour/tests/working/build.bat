@echo off
make -r "PRG_SOURCES=%1.prg"
if not errorlevel 1 %hb_architecture\%hb_compiler\%1 %2 %3 %4 %5 %6 %7 %8 %9
