# $Id$
# Makefile for Watcom C/C++ 10.x
#
!include ..\..\makewat.env

TARGET=$(HARBOURDIR)\bin\harbour.exe

all : $(TARGET)

$(TARGET) : y_tab.obj lexyy.obj harbour.obj
  %create link.tmp
  %append link.tmp $(WLDEBUG)
  %append link.tmp FI y_tab
  %append link.tmp FI lexyy
  %append link.tmp FI harbour
  %append link.tmp NAME $(TARGET)
  %append link.tmp $(WLOPTIONS)
  %append link.tmp $(WLSTACK)
  wlink @link.tmp

y_tab.obj : y_tab.c
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

lexyy.obj : lexyy.c
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

harbour.obj : harbour.c
    *$(WC) $(WCOPTIONS) $(WCINCLUDE) $(WCDEBUG) $(WCDEFINE) $(WCEXTRA) $<

y_tab.c : harbour.y
   bison -d -v -o y_tab.c harbour.y

lexyy.c : harbour.l
   flex -i -8 -olexyy.c harbour.l

clean : .SYMBOLIC
  del *.out
  del *.obj
  del y_tab.*
  del lexyy.*
  del *.h
  del *.err
  del *.tmp
