@echo off

REM From .PRG to .C = Harbour
..\..\bin\harbour %1.prg /n /i..\..\include
if errorlevel 1 goto end

REM From .C to .EXE = BuildExe
call BLD32EXE %1 %2

:end
