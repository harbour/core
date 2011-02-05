#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbzlib

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   adler32.c \
   compress.c \
   crc32.c \
   deflate.c \
   gzclose.c \
   gzlib.c \
   gzread.c \
   gzwrite.c \
   infback.c \
   inffast.c \
   inflate.c \
   inftrees.c \
   trees.c \
   uncompr.c \
   zutil.c \

ifneq ($(HB_HAS_ZLIB_LOCAL),)
   # To suppress new warnings in version 1.2.5
   ifneq ($(filter $(HB_COMPILER),mingw mingw64 mingwarm cygwin djgpp),)
      HB_CFLAGS += -DNO_VIZ
   endif
   ifeq ($(HB_COMPILER),bcc)
      HB_CFLAGS_DYN += -DZLIB_DLL -DZLIB_INTERNAL
   endif
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := unused
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN http://www.zlib.net/
# VER    1.2.5
# URL    http://zlib.net/zlib125.zip
# DIFF   zlib.dif
#
# MAP    README
# MAP    adler32.c
# MAP    compress.c
# MAP    crc32.c
# MAP    crc32.h
# MAP    deflate.c
# MAP    deflate.h
# MAP    gzclose.c
# MAP    gzguts.h
# MAP    gzlib.c
# MAP    gzread.c
# MAP    gzwrite.c
# MAP    infback.c
# MAP    inffast.c
# MAP    inffast.h
# MAP    inffixed.h
# MAP    inflate.c
# MAP    inflate.h
# MAP    inftrees.c
# MAP    inftrees.h
# MAP    trees.c
# MAP    trees.h
# MAP    uncompr.c
# MAP    zconf.h
# MAP    zlib.h
# MAP    zutil.c
# MAP    zutil.h
