@echo off
rem
rem $Id$
rem

IF NOT EXIST hscript.exe GOTO :missing

:start
SET HARBOURDIR=.\

hscript hello.hs    > hello.htm
hscript multiply.hs > multiply.htm
hscript dir.hs      > dir.htm
hscript ugly.hs     > ugly.htm

cls
echo Ready to go!
echo\
echo If you're under W95/98 try:
echo\
echo start hello.htm
echo -or-
echo start multiply.htm
echo -or-
echo start dir.htm

goto end

:missing
echo Missing hscript.exe
echo\
echo Press any key to build it or Ctrl+C to quit...
pause>nul
call makehs
goto :start

:end
