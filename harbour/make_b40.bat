@echo off
rem 
rem $Id$
rem 

make -DB40 -fmakefile.b32 >> make.err
notepad make.err
