@echo off
rem
rem $Id$
rem

set CFLAGS=-Iinclude -D_RWSTD_NO_NEW_HEADER

call ..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

set CFLAGS=
