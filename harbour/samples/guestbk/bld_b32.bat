@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n guestbk
..\..\bin\harbour /n ..\..\tests\inifiles
..\..\bin\harbour /n ..\..\tests\testcgi

echo -O2 -eguestbk.exe -I..\..\include guestbk.c > temp.bld
echo inifiles.c testcgi.c >> temp.bld
echo ..\..\libs\b32\harbour.lib  ..\..\libs\b32\terminal.lib >> temp.bld
echo ..\..\libs\b32\hbgt.lib ..\..\libs\b32\hbpp.lib >> temp.bld
echo ..\..\libs\b32\rdd.lib >> temp.bld
bcc32 @temp.bld
del temp.bld

guestbk
