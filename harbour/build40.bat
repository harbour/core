@echo off
del build40.err
make -fhbpplib.b32  > build40.err
make -fmakefile.b40 >> build40.err
make -fterminal.b32 >> build40.err
call bldgt32.bat >> build40.err
call bldhbpp.bat >> build40.err
cd tests\working
call bld32exe.bat ..\..\source\runner\runner runner >> build40.err
cd ..\..
edit build40.err
