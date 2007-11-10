#
# $Id$
#

#
# Borland C++ IDE generated makefile
# Generated 11/04/01 at 19:22:16 
#
.AUTODEPEND


#
# Borland C++ tools
#
!ifndef BCB
BCB = .
!endif

OBJ_DIR = obj\b32
LIB_DIR = ..\..\lib\b32

IMPLIB  = Implib
BCC32   = Bcc32 -vi -Ve -6 -R- -H- -5 -OS -w- -O2 -X- -a8 -b -k-
TLINK32 = TLink32
TLIB    = TLib
BRC32   = Brc32
TASM32  = Tasm32
#
# IDE macros
#


#
# Options
#
IDE_LinkFLAGS32 =  -L$(BCB)\LIB\b32
LinkerLocalOptsAtC32_ziparddll =  -L$(BCB)\LIB\b32 -Tpe -ap -c
ResLocalOptsAtC32_ziparddll = 
BLocalOptsAtC32_ziparddll = 
CompInheritOptsAt_ziparddll = -I$(BCB)\INCLUDE -Iinclude -I..\..\include;. -DZLIB_DLL;WIN32;ASSERT -u 
LinkerInheritOptsAt_ziparddll = -x
LinkerOptsAt_ziparddll = $(LinkerLocalOptsAtC32_ziparddll)
ResOptsAt_ziparddll = $(ResLocalOptsAtC32_ziparddll)
BOptsAt_ziparddll = $(BLocalOptsAtC32_ziparddll) 

#
# Dependency List
#
Dep_zipar = \
   $(LIB_DIR)\hbzip.lib

ziparchive : $(Dep_zipar)
  echo MakeNode

CLEAN:
   -@if exist $(Dep_zipar) erase $(Dep_zipar)
   -@if exist $(OBJ_DIR)\*.obj erase $(OBJ_DIR)\*.obj
   -@if exist $(OBJ_DIR)\*.c   erase $(OBJ_DIR)\*.c
   -@if exist $(OBJ_DIR)\*.res erase $(OBJ_DIR)\*.res
   -@if exist $(OBJ_DIR)\*.map erase $(OBJ_DIR)\*.map
   -@if exist $(OBJ_DIR)\*.rws erase $(OBJ_DIR)\*.rws

Dep_ziparddll = \
   $(OBJ_DIR)\ziparchive.obj\
   $(OBJ_DIR)\zip.obj\
   $(OBJ_DIR)\hbcomprs.obj\
   $(OBJ_DIR)\zipplatform.obj\
   $(OBJ_DIR)\zipstorage.obj\
   $(OBJ_DIR)\zipstring.obj\
   $(OBJ_DIR)\zipplatformcomm.obj\
   $(OBJ_DIR)\zippathcomponent.obj\
   $(OBJ_DIR)\zipmemfile.obj\
   $(OBJ_DIR)\zipfileheader.obj\
   $(OBJ_DIR)\zipfile.obj\
   $(OBJ_DIR)\zipnew.obj\
   $(OBJ_DIR)\zipcomp.obj\
   $(OBJ_DIR)\zipexception.obj\
   $(OBJ_DIR)\zipcompatibility.obj\
   $(OBJ_DIR)\zipcentraldir.obj\
   $(OBJ_DIR)\zipautobuffer.obj\
   $(OBJ_DIR)\stdafx.obj

$(LIB_DIR)\hbzip.lib : $(Dep_ziparddll)
  $(TLIB) $< $(IDE_BFLAGS) /P64  $(BOptsAt_ziparddll) @&&|
 -+$(OBJ_DIR)\zip.obj &
-+$(OBJ_DIR)\hbcomprs.obj &
-+$(OBJ_DIR)\zipstorage.obj &
-+$(OBJ_DIR)\zipstring.obj &
-+$(OBJ_DIR)\zipplatformcomm.obj &
-+$(OBJ_DIR)\zipplatform.obj &
-+$(OBJ_DIR)\zippathcomponent.obj &
-+$(OBJ_DIR)\zipmemfile.obj &
-+$(OBJ_DIR)\zipfileheader.obj &
-+$(OBJ_DIR)\zipfile.obj &
-+$(OBJ_DIR)\zipexception.obj &
-+$(OBJ_DIR)\zipcompatibility.obj &
-+$(OBJ_DIR)\zipcentraldir.obj &
-+$(OBJ_DIR)\zipautobuffer.obj &
-+$(OBJ_DIR)\ziparchive.obj &
-+$(OBJ_DIR)\zipnew.obj &
-+$(OBJ_DIR)\zipcomp.obj &
-+$(OBJ_DIR)\stdafx.obj
|


$(OBJ_DIR)\zip.obj :  zip.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -I\xharbour\include -DZLIB_DLL -o$@ zip.c
|

$(OBJ_DIR)\hbcomprs.obj :  hbcomprs.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -I\xharbour\include -DZLIB_DLL -o$@ hbcomprs.c
|

$(OBJ_DIR)\zipstorage.obj :  zipstorage.cpp
  $(BCC32) -c  @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipstorage.cpp
|

$(OBJ_DIR)\zipstring.obj :  zipstring.cpp
  $(BCC32) -c  @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipstring.cpp
|

$(OBJ_DIR)\zipplatformcomm.obj :  zipplatformcomm.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipplatformcomm.cpp
|

$(OBJ_DIR)\zipplatform.obj :  zipplatform.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipplatform.cpp
|

$(OBJ_DIR)\zippathcomponent.obj :  zippathcomponent.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zippathcomponent.cpp
|

$(OBJ_DIR)\zipmemfile.obj :  zipmemfile.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipmemfile.cpp
|

$(OBJ_DIR)\zipnew.obj :  zipnew.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipnew.cpp
|

$(OBJ_DIR)\zipcomp.obj :  zipcomp.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipcomp.cpp
|

$(OBJ_DIR)\zipfileheader.obj :  zipfileheader.cpp
  $(BCC32) -c  @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipfileheader.cpp
|

$(OBJ_DIR)\zipfile.obj :  zipfile.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipfile.cpp
|

$(OBJ_DIR)\zipexception.obj :  zipexception.cpp
  $(BCC32)  -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipexception.cpp
|

$(OBJ_DIR)\zipcompatibility.obj :  zipcompatibility.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipcompatibility.cpp
|

$(OBJ_DIR)\zipcentraldir.obj :  zipcentraldir.cpp
  $(BCC32) -c   @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipcentraldir.cpp
|

$(OBJ_DIR)\zipautobuffer.obj :  zipautobuffer.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipautobuffer.cpp
|

$(OBJ_DIR)\ziparchive.obj :  ziparchive.cpp
  $(BCC32) -c   @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ ziparchive.cpp
|

$(OBJ_DIR)\stdafx.obj :  stdafx.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ stdafx.cpp
|

# Compiler configuration file
BccW32.cfg : 
   Copy &&|
-vi
-Ve
-6
-R-
-H-
-5
-OS
-w-
-O2  -X- -a8 -b -k-  
| $@

