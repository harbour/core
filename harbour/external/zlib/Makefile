#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no

C_SOURCES := \
   adler32.c \
   compress.c \
   crc32.c \
   deflate.c \
   gzio.c \
   infback.c \
   inffast.c \
   inflate.c \
   inftrees.c \
   trees.c \
   uncompr.c \
   zutil.c \

LIBNAME := hbzlib

ifeq ($(HB_WITH_ZLIB),)
   ifneq ($(filter $(HB_PLATFORM),win wce os2 dos),)
      HB_WITH_ZLIB := yes
   endif
endif

ifeq ($(HB_WITH_ZLIB),yes)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := not supported or disabled
   include $(TOP)$(ROOT)config/none.mk
endif
