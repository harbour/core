rem 
rem $Id$
rem 

@echo off

if %1.==hb. make -fharbour.b31 -r %2
if not %1.==hb. make -fmakefile.b31 -r %1
