@echo off
rem 
rem $Id$
rem 

nmake /f makefile.vc %1 > make_vc.log
notepad make_vc.log
