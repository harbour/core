@echo off
rem 
rem $Id$
rem 

if exist buildvc.err del buildVC.err
nmake /f makefile.vc %1 > BuildVC.err
notepad buildVC.err
