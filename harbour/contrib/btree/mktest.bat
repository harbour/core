@echo off

cd test\dos\djgpp\
del test.exe
del *.c

..\..\..\..\..\source\compiler\dos\djgpp\harbour.exe ../../test.prg  -n -q0 -w -es2 -gc0 -I../../ -I../../../ -I../../../../../include
gcc -I. -I../../../../../include -Wall  -c test.c -otest.o

..\..\..\..\..\source\compiler\dos\djgpp\harbour.exe ../../ttest.prg  -n -q0 -w -es2 -gc0 -I../../ -I../../../ -I../../../../../include
gcc -I. -I../../../../../include -Wall  -c ttest.c -ottest.o

gcc -I. -I../../../../../include -I../../.. -Wall -c ../../ctest.c -octest.o

gcc @__link__.tmp
cd ..\..\..

pause
test\dos\djgpp\test

:done