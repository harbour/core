@echo off
rem 
rem $Id$
rem 

make -fhbpplib.b32  > build32.err
make -fmakefile.b32 >> build32.err
make -fterminal.b32 >> build32.err
make -fgt.b32 >> build32.err
make -frdd.b32 >> build32.err
rem make -fhbpp.b32 >> build32.err
make -frunner.b32 >> build32.err
edit build32.err
