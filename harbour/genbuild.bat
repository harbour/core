@echo off
rem 
rem $Id$
rem 

cd..
pkzip -r -p harb21-2 -x*.exe -x*.bak -x*.sim -x*.out -xsave.bat -xlexyy.c -xy_tab.c -xy_tab.h -x*.obj -x*.lib -x*.zip
