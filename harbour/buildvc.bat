rem 
rem $Id$
rem 

@echo off

del buildVC.err
masm source\compiler\symbols.asm obj\symbols.obj obj\symbols.lst obj\symbols.crf > BuildVC.err
nmake /f makefile.vc >> BuildVC.err
notepad buildVC.err