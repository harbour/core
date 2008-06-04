@echo off
rem
rem $Id$
rem

rem ; sqlite code has lots of warnings, suppress them
set CFLAGS=-w-

call ..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
