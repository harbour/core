@echo off
rem 
rem $Id$
rem 

del make.err
make -fmakefile.b32 >> make.err
notepad make.err
