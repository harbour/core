@echo off
rem 
rem $Id$
rem 

REM From .PRG to .C = Harbour
..\..\bin\harbour %1 /n /gHRB /i..\..\include
hbrun %1
