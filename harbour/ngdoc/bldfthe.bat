@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 /n /iinclude /p
bcc32 -e%1.exe -O2 -M -v  -I..\include -L..\lib\b32  harbour.lib terminal.lib hbpp.lib hbgt.lib common.lib ..\lib\b32\rdd.lib %1.c
rem del %1.c
