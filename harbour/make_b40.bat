@echo off
rem 
rem $Id$
rem 

make -fhbpplib.b32  > build40.err
make -fmakefile.b40 >> build40.err
make -fterminal.b32 >> build40.err
make -fgt.b32 >> build40.err
make -frdd.b32 >> build40.err
rem make -fhbpp.b32 >> build40.err
make -frunner.b32 >> build40.err
Notepad build40.err
