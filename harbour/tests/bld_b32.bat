@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 /n /i..\include
bcc32 -e%1.exe -O2 -I..\include -L..\lib\b32 -v harbour.lib terminal.lib hbpp.lib hbgt.lib common.lib rdd.lib %1.c
rem del %1.c
