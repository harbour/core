@echo off
rem 
rem $Id$
rem 

if exist buildvc.err del buildVC.err
masm source\vm\symbols.asm obj\symbols.obj obj\symbols.lst obj\symbols.crf > BuildVC.err
nmake /f makefile.vc %1 >> BuildVC.err
notepad buildVC.err