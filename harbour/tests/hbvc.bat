@echo off
rem 
rem $Id$
rem 

REM From .PRG to .C = Harbour
..\..\bin\harbour %1 /n /i..\..\include

REM From .C to .EXE = BuildExe
call BLDVCEXE %1 %2
