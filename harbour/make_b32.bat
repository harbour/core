@echo off
rem 
rem $Id$
rem 

make -fmakefile.b32 > make_b32.err
notepad make_b32.err
