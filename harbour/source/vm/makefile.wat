# $Id$
# Makefile for Watcom C/C++ 10.x
#
!include ..\..\makewat.env

TARGET=$(HARBOURLIB)

OBJECTS=hvm.obj dynsym.obj initsymb.obj

all : $(TARGET)

$(TARGET) : $(OBJECTS)
  %create lib.tmp
  @for %i in ( $(OBJECTS) ) do @%append lib.tmp +-%i
  wlib $^@ @lib.tmp

clean : .SYMBOLIC
  -del *.obj
  -del *.tmp
  -del *.err
