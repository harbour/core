@echo off
rem
rem $Id$
rem

set HB_SHOW_ERRORS=no

set DO_NOT_COMPILE=examples hbclip hgf msql
set DIRS=adordd bmdbfcdx btree libct libgt libmisc libnf ole pdflib samples tip win32 xhb firebird freeimage gd apollo directx hbzlib htmllib mysql odbc pgsql rdd_ads telepath

FOR %%n IN ( %DIRS% ) DO call make_vc.bat %%n %1 %2 %3 %4 %5 %6 %7 %8 %9
