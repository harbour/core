@echo off
rem 
rem $Id$
rem 

nmake /f makefile.vc %1 > make_vc.err
notepad make_vc.err
