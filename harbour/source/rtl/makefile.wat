# $Id$
# Makefile for Watcom C/C++ 10.x
#
!include ..\..\makewat.env

TARGET=$(HARBOURLIB)

PRGSYS=tclass.prg errorsys.prg error.prg asort.prg objfunc.prg
OBJSYS=tclass.obj errorsys.obj error.obj asort.obj objfunc.obj
OBJECTS=arrays.obj console.obj dates.obj extend.obj strings.obj classes.obj &
        strcmp.obj files.obj set.obj math.obj environ.obj itemapi.obj &
        transfrm.obj errorapi.obj codebloc.obj $(OBJSYS)

all: $(TARGET)

$(TARGET): $(OBJECTS)
  %create lib.tmp
  @for %i in ( $(OBJECTS) ) do @%append lib.tmp +-%i
  wlib $^@ @lib.tmp

clean: .SYMBOLIC
  del *.obj 
  del *.tmp 
  del *.err
