@echo off
del build32.err
make -fhbpplib.b32  > build32.err
make -fmakefile.b32 >> build32.err
make -fterminal.b32 >> build32.err
call bldgt32.bat >> build32.err
call bldhbpp.bat >> build32.err
call buildrdd.bat >> build32.err
rem make -fhbpp.b32 >> build32.err
make -frunner.b32 >> build32.err
edit build32.err
