@echo off
rem 
rem $Id$
rem 

call run_prg.bat test_all
call test_all.bat
del test_all.bat
