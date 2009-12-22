@rem
@rem $Id$
@rem

@echo off

@rem has to be run from contrib root. Adjust target dir.

pushd

cd ..\..\contrib

for /F %%a in ( 'dir /b /ad' ) do (
   echo %%a
   xcopy /y /s %%a\*.hbc     F:\hb\hb20\contrib\%%a\
   xcopy /y /s %%a\tests\*.* F:\hb\hb20\contrib\%%a\tests\
   xcopy /y /s %%a\utils\*.* F:\hb\hb20\contrib\%%a\utils\
)

for /F %%a in ( 'dir /b /ad rddsql' ) do (
   echo %%a
   xcopy /y /s rddsql\%%a\*.hbc     F:\hb\hb20\contrib\rddsql\%%a\
   xcopy /y /s rddsql\%%a\tests\*.* F:\hb\hb20\contrib\rddsql\%%a\tests\
)

popd
