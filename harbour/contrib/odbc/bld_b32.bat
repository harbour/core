@echo off
rem
rem $Id$
rem

..\..\bin\harbour %1 /n /i..\include %2
bcc32 -e%1.exe -O2 -I..\..\include -L..\..\lib\b32 -v harbour.lib terminal.lib hbpp.lib hbgt.lib rdd.lib hbodbc.lib odbc32.lib %1.c
rem del %1.c
