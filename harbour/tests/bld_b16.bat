@echo off
rem
rem $Id$
rem

..\bin\harbour %1 /n /i..\include
bcc %1.c -e%1.exe -O2 -I..\include -L..\lib\b16 -P -mh -Fm harbour.lib terminal.lib hbpp.lib hbtools.lib
del %1.c
