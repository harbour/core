@echo off
rem 
rem $Id$
rem 

make -DB40 -fcommon.b32 > make.err
make -DB40 -fhbpp.b32 > make.err
make -DB40 -fmakefile.b32 >> make.err
make -DB40 -fterminal.b32 >> make.err
make -DB40 -fgt.b32 >> make.err
make -DB40 -frdd.b32 >> make.err
make -DB40 -frunner.b32 >> make.err
make -DB40 -fregress.b32 >> make.err
make -DB40 -fft_helpc.b32 >> make.err
notepad make.err
