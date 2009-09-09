@rem
@rem $Id$
@rem

@echo off

@rem has to be run from contrib root. Adjust target dir.

for /F %%a in ( 'dir /b /ad' ) do (
   echo %%a
   xcopy /y /s %%a\*.hbc     C:\hb\hb20\contrib\%%a\
   xcopy /y /s %%a\tests\*.* C:\hb\hb20\contrib\%%a\tests\
   xcopy /y /s %%a\utils\*.* C:\hb\hb20\contrib\%%a\utils\
)

for /F %%a in ( 'dir /b /ad rddsql' ) do (
   echo %%a
   xcopy /y /s rddsql\%%a\*.hbc     C:\hb\hb20\contrib\rddsql\%%a\
   xcopy /y /s rddsql\%%a\tests\*.* C:\hb\hb20\contrib\rddsql\%%a\tests\
)

rem xcopy /D /Y source\hbzlib\*.h %HB_INSTALL_PREFIX%\source\hbzlib\
rem xcopy /D /Y source\hbpcre\*.h %HB_INSTALL_PREFIX%\source\hbpcre\
