# $Id$
# Makefile for Watcom C/C++ 10.x
#
!include ..\..\makewat.env

TARGET=$(HARBOURLIB)

OBJECTS=datesx.obj stringsx.obj mathx.obj

all: $(TARGET)

$(TARGET): $(OBJECTS) $(HARBSYS)
  %create lib.tmp
  @for %i in ( $(OBJECTS) ) do @%append lib.tmp +-%i
  wlib $^@ @lib.tmp

clean: .SYMBOLIC
  del *.obj 
  del *.tmp 
  del *.err
