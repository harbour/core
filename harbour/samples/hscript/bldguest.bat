@echo off

..\..\bin\harbour guestbk /n
..\..\bin\harbour ..\inifiles
..\..\bin\harbour ..\testcgi

echo -O2 -eguestbk.exe -I..\..\include ..\..\source\vm\hvm.c guestbk.c > b32.bc
echo inifiles.c testcgi.c                                        >> b32.bc
echo ..\..\libs\b32\harbour.lib  ..\..\libs\b32\terminal.lib >> b32.bc
echo ..\..\libs\b32\hbgt.lib ..\..\libs\b32\hbpp.lib >> b32.bc
echo ..\..\libs\b32\rdd.lib >> b32.bc
bcc32 @b32.bc
del b32.bc

guestbk
