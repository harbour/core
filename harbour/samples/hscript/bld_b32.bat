@echo off
rem
rem $Id$
rem

..\..\bin\harbour hscript /n /i..\..\include
bcc32 -ehscript.exe -O2 -I..\..\include -L..\..\lib\b32 -v harbour.lib terminal.lib hbpp.lib hbgt.lib rdd.lib hscript.c
rem del hscript.c
