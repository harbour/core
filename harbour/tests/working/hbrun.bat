@echo off
rem 
rem $Id$
rem 

REM From .PRG to .C = Harbour
..\..\bin\harbour %1.prg /n /gHRB /i..\..\include
runner %1.hrb
