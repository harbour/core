@rem
@rem $Id$
@rem

@echo off

rem ; Generate test_all.bat
call run_prg.bat test_all

rem ; Run test_all.bat
call test_all.bat
del test_all.bat
