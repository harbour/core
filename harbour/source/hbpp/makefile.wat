# $Id$
# Makefile for Watcom C/C++ 10.x
#
!include ..\..\makewat.env

TARGET=$(HARBOURDIR)\libs\hbpp.lib

OBJECTS=hbpp.obj hbppint.obj table.obj

all : $(TARGET)

.c.obj: # $< # .AUTODEPEND
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

$(TARGET) : $(OBJECTS)
  %create lib.tmp
  @for %i in ( $(OBJECTS) ) do @%append lib.tmp +-%i
  wlib $^@ @lib.tmp

clean : .SYMBOLIC
  -del *.obj
  -del *.tmp
  -del *.err
