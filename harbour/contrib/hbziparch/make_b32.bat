@echo off
rem
rem $Id$
rem

set CFLAGS=-w-inl- -w-stl- -w-ncf- -Iinclude

call ..\mtpl_b32.bat %1 %2 %3 %4 %5 %6 %7 %8 %9

set CFLAGS=
