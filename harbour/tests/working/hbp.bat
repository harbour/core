@echo off

REM From .PRG to .C = Harbour
..\..\bin\harbour %1.prg /n /i..\..\include
if errorlevel 1 goto end

REM From .C to .EXE = Bld+Exe
call bldpexe %1 %2

:end
