rem 
rem $Id$
rem 

@echo off

REM From .PRG to .C = Harbour
..\..\bin\harbour %1.prg %2 /n /i..\..\include
if errorlevel 1 goto end

REM From .C to .EXE = BuildExe
call BLD32EXE %1

:end
