# $Id$
# Makefile for Watcom C/C++
#
all : hbpp compiler vm rtl tools

hbpp : .SYMBOLIC
   cd hbpp
   wmake $(__MAKEOPTS__) /f makefile.wat all
   cd ..

compiler : .SYMBOLIC
   cd compiler
   wmake $(__MAKEOPTS__) /f makefile.wat all
   cd ..

vm : .SYMBOLIC
   cd vm
   wmake $(__MAKEOPTS__) /f makefile.wat all
   cd ..

rtl : .SYMBOLIC
   cd rtl
   wmake $(__MAKEOPTS__) /f makefile.wat all
   cd ..

tools: .SYMBOLIC
   cd tools
   wmake $(__MAKEOPTS__) /f makefile.wat all
   cd ..

clean : .SYMBOLIC
   cd compiler
   wmake /f makefile.wat clean
   cd ../vm
   wmake /f makefile.wat clean
   cd ../rtl
   wmake /f makefile.wat clean
   cd ..
