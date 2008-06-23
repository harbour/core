@echo off
rem
rem $Id$
rem

set CFLAGS=-EHsc -D_CRT_NONSTDC_NO_DEPRECATE

call ..\mtpl_vc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

set CFLAGS=
