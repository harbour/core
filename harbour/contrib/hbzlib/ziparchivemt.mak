#
# Borland C++ IDE generated makefile
# Generated 11/04/01 at 19:22:16 
#
.AUTODEPEND


#
# Borland C++ tools
#
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif

IMPLIB  = Implib
BCC32   = Bcc32 +BccW32.cfg 
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
IDE_LinkFLAGS32 =  -L$(BCB)\LIB
LinkerLocalOptsAtC32_ziparddll =  -L$(BCB)\LIB -Tpe -ap -c
ResLocalOptsAtC32_ziparddll = 
BLocalOptsAtC32_ziparddll = 
CompInheritOptsAt_ziparddll = -I$(BCB)\INCLUDE -DWIN32;ZLIB_DLL -u $(CFLAGS)
LinkerInheritOptsAt_ziparddll = -x
LinkerOptsAt_ziparddll = $(LinkerLocalOptsAtC32_ziparddll)
ResOptsAt_ziparddll = $(ResLocalOptsAtC32_ziparddll)
BOptsAt_ziparddll = $(BLocalOptsAtC32_ziparddll) 

#
# Dependency List
#
Dep_zipar = \
   ziparchivemt.lib

ziparchive : BccW32.cfg $(Dep_zipar)
  echo MakeNode

Dep_ziparddll = \
   ziparchive.obj\
   zutil.obj\
   uncompr.obj\
   adler32.obj\
   infutil.obj\
   inftrees.obj\
   inflate.obj\
   inffast.obj\
   infcodes.obj\
   infblock.obj\
   gzio.obj\
   trees.obj\
   deflate.obj\
   crc32.obj\
   compress.obj\
   zipplatform.obj\
   zipstorage.obj\
   zipplatformcomm.obj\
   zippathcomponent.obj\
   zipmemfile.obj\
   zipinternalinfo.obj\
   zipfileheader.obj\
   zipfile.obj\
   zipexception.obj\
   zipcompatibility.obj\
   zipcollections.obj\
   zipcentraldir.obj\
   zipautobuffer.obj\
   stdafx.obj

ziparchivemt.lib : $(Dep_ziparddll)
  $(TLIB) $< $(IDE_BFLAGS) /P32  $(BOptsAt_ziparddll) @&&|
 -+zutil.obj &
-+uncompr.obj &
-+adler32.obj &
-+trees.obj &
-+infutil.obj &
-+inftrees.obj &
-+inflate.obj &
-+inffast.obj &
-+infcodes.obj &
-+infblock.obj &
-+gzio.obj &
-+deflate.obj &
-+crc32.obj &
-+compress.obj &
-+makebcdll.obj &
-+zipstorage.obj &
-+zipplatformcomm.obj &
-+zipplatform.obj &
-+zippathcomponent.obj &
-+zipmemfile.obj &
-+zipinternalinfo.obj &
-+zipfileheader.obj &
-+zipfile.obj &
-+zipexception.obj &
-+zipcompatibility.obj &
-+zipcollections.obj &
-+zipcentraldir.obj &
-+zipautobuffer.obj &
-+ziparchive.obj &
-+stdafx.obj
|

zutil.obj :  zutil.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL -o$@ zutil.c
|

uncompr.obj :  uncompr.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL -o$@ uncompr.c
|

adler32.obj :  adler32.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ adler32.c
|

trees.obj :  trees.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ trees.c
|

infutil.obj :  infutil.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ infutil.c
|

inftrees.obj :  inftrees.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ inftrees.c
|

inflate.obj :  inflate.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ inflate.c
|

inffast.obj :  inffast.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ inffast.c
|

infcodes.obj :  infcodes.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ infcodes.c
|

infblock.obj :  infblock.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ infblock.c
|

gzio.obj :  gzio.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ gzio.c
|

deflate.obj :  deflate.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ deflate.c
|

crc32.obj :  crc32.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ crc32.c
|

compress.obj :  compress.c
  $(BCC32) -P- -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -DZLIB_DLL  -o$@ compress.c
|

zipstorage.obj :  zipstorage.cpp
  $(BCC32) -c  @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipstorage.cpp
|

zipplatformcomm.obj :  zipplatformcomm.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipplatformcomm.cpp
|

zipplatform.obj :  zipplatform.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipplatform.cpp
|

zippathcomponent.obj :  zippathcomponent.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zippathcomponent.cpp
|

zipmemfile.obj :  zipmemfile.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipmemfile.cpp
|

zipinternalinfo.obj :  zipinternalinfo.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipinternalinfo.cpp
|

zipfileheader.obj :  zipfileheader.cpp
  $(BCC32) -c  @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipfileheader.cpp
|

zipfile.obj :  zipfile.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipfile.cpp
|

zipexception.obj :  zipexception.cpp
  $(BCC32)  -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipexception.cpp
|

zipcompatibility.obj :  zipcompatibility.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipcompatibility.cpp
|

zipcollections.obj :  zipcollections.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipcollections.cpp
|

zipcentraldir.obj :  zipcentraldir.cpp
  $(BCC32) -c   @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipcentraldir.cpp
|

zipautobuffer.obj :  zipautobuffer.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ zipautobuffer.cpp
|

ziparchive.obj :  ziparchive.cpp
  $(BCC32) -c   @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ ziparchive.cpp
|

stdafx.obj :  stdafx.cpp
  $(BCC32) -c @&&|
 $(CompOptsAt_ziparddll) $(CompInheritOptsAt_ziparddll) -o$@ stdafx.cpp
|

# Compiler configuration file
BccW32.cfg : 
   Copy &&|
-vi
-Ve
-R-
-H-
-5
-OS
-w-
-O2  -X- -a8 -b -k-  
| $@


