@echo off
rem
rem $Id$
rem

..\bin\harbour %1 /n /i..\include
bcc32 %1.c -e%1.exe -O2 -I..\include -L..\lib\b32 -v harbour.lib terminal.lib hbpp.lib hbgt.lib rdd.lib
del %1.c
