@echo off

REM From .PRG to .C = Harbour
..\..\bin\harbour %1.prg /n

REM From .C to .EXE = BuildExe
call BLDVCEXE %1 %2
