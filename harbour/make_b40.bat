@echo off
rem 
rem $Id$
rem 

make -DB40 -fmakefile.b32 > make_b40.err
notepad make_b40.err
