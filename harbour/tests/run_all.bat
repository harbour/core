@echo off
rem 
rem $Id$
rem 

..\bin\harbour test_all.prg /n /gHBR /i..\include
hbrun test_all
call testall.bat
