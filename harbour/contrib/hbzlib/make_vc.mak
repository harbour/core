#
# $Id$
#

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

OUTDIR=..\..\lib\vc
INTDIR=obj\vc

ALL : "$(OUTDIR)\hbzip.lib"

CLEAN :
	-@if exist "$(INTDIR)\*.obj" del "$(INTDIR)\*.obj" >nul
	-@if exist "$(INTDIR)\*.c" del "$(INTDIR)\*.c" >nul
	-@if exist "$(OUTDIR)\hbzip.lib" del "$(OUTDIR)\hbzip.lib" >nul

"$(OUTDIR)" :
    @if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    @if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=-Ogt2yb1p -FD -GA -GB -Gs -W3 /nologo /GX /ML /I "..\..\include" /Iinclude\
         /DWIN32 /D_WIN32 /DNDEBUG /D_MBCS /D_LIB\
         /Fp"$(INTDIR)\hbzip.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
         /FD /c $(CFLAGS)

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $<
<<

LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\hbzip.lib"
LIB32_OBJS= \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\zip.obj" \
	"$(INTDIR)\hbcomprs.obj" \
	"$(INTDIR)\ziparchive.obj" \
	"$(INTDIR)\zipautobuffer.obj" \
	"$(INTDIR)\zipcentraldir.obj" \
	"$(INTDIR)\zipcomp.obj" \
	"$(INTDIR)\zipcompatibility.obj" \
	"$(INTDIR)\zipexception.obj" \
	"$(INTDIR)\zipfile.obj" \
	"$(INTDIR)\zipfileheader.obj" \
	"$(INTDIR)\zipmemfile.obj" \
	"$(INTDIR)\zipnew.obj" \
	"$(INTDIR)\zippathcomponent.obj" \
	"$(INTDIR)\zipplatform.obj" \
	"$(INTDIR)\zipplatformcomm.obj" \
	"$(INTDIR)\zipstorage.obj" \
	"$(INTDIR)\ZipString.obj"

"$(OUTDIR)\hbzip.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

