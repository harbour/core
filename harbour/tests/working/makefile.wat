# Makefile for WATCOM C/C++ 10.x
#
!ifdef FILE
!include ..\..\makewat.env

all: $(FILE).exe

$(FILE).exe : $(FILE).obj $(HARBOURDIR)\bin\harbour.exe
  %create link.tmp
  %append link.tmp $(WLDEBUG)
  %append link.tmp FI $(FILE)
  %append link.tmp NAME $(FILE)
  %append link.tmp LIBRARY $(WLLIBS)
  %append link.tmp $(WLOPTIONS)
  %append link.tmp $(WLSTACK)
  wlink @link.tmp

$(FILE).c : $(FILE).prg
  $(HARBOURDIR)\bin\harbour $(FILE) -n

!else
dummy : .SYMBOLIC
  @echo =============================================================
  @echo Please give a name of PRG file to compile (without extension)
  @echo wmake /f makefile.wat FILE=codebloc
!endif