# $Id$
# Makefile for Watcom C/C++ 10.x
#
!include ..\..\makewat.env

TARGET=$(HARBOURDIR)\bin\harbour.exe

all : $(TARGET)

$(TARGET) : harboury.obj harbourl.obj harbour.obj
  %create link.tmp
  %append link.tmp $(WLDEBUG)
  %append link.tmp FI harboury
  %append link.tmp FI harbourl
  %append link.tmp FI harbour
  %append link.tmp NAME $(TARGET)
  %append link.tmp $(WLOPTIONS)
  %append link.tmp $(WLSTACK)
  wlink @link.tmp

harboury.obj : harboury.c
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

harbourl.obj : harbourl.c
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

harbour.obj : harbour.c
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

harboury.c : harbour.y
   bison -d -v -o harboury.c harbour.y

harbourl.c : harbour.l
   flex -i -8 -oharbourl.c harbour.l

clean : .SYMBOLIC
  del *.out
  del *.obj
  del harboury.*
  del harbourl.*
  del *.h
  del *.err
  del *.tmp
