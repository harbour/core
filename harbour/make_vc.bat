@echo off
rem 
rem $Id$
rem 

nmake /f makefile.vc %1 > make.err
notepad make.err
