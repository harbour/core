@echo off
rem 
rem $Id$
rem 

del make.err
make -fhbpp.b32 >> make.err
make -fmakefile.b32 >> make.err
make -fterminal.b32 >> make.err
make -fgt.b32 >> make.err
make -frdd.b32 >> make.err
make -frunner.b32 >> make.err
make -fregress.b32 >> make.err
notepad make.err
