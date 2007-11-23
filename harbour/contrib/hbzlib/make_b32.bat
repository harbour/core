@echo off
rem
rem $Id$
rem

set CFLAGS=%C_USR% -Iinclude -DZLIB_DLL;WIN32;ASSERT -vi -Ve -6 -R- -H- -5 -OS -w- -X- -a8 -b -k-

call ..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
