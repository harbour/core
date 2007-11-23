@echo off
rem
rem $Id$
rem

set CFLAGS=%C_USR% -Iinclude -DZLIB_DLL;WIN32;ASSERT

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
