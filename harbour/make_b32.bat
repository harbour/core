@echo off
rem 
rem $Id$
rem 

make -fmakefile.b32 > make.err
notepad make.err
