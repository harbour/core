@echo off
rem
rem $Id$
rem

rem ; sqlite won't compile in C++ mode.
set CFLAGS=-TC

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
