#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbzlib

HB_BUILD_WARN := no
HB_BUILD_MODE := c

HB_CFLAGS_DYN := -DZLIB_DLL

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
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := unused
   include $(TOP)$(ROOT)config/none.mk
endif
