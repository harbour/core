@echo off
rem del build40.err
make -fhbpplib.b32  > build40.err
make -fmakefile.b40 >> build40.err
make -fterminal.b32 >> build40.err
call bldgt32.bat >> build40.err
call bldhbpp.bat >> build40.err
call buildrdd.bat >> build40.err
make -frunner.b32 >> build40.err
Notepad build40.err
