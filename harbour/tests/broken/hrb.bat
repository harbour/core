@echo off

REM From .PRG to .C = Harbour
..\..\bin\harbour %1.prg /n /gh
runner %1.hrb
