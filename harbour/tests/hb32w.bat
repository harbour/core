@echo off
rem 
rem $Id$
rem 

REM From .PRG to .C = Harbour
..\..\bin\harbour %1 /n /i..\..\include
if errorlevel 1 goto end

REM From .C to .EXE = BuildExe
call BLD32W %1 %2

:end
