#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbzlib

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

ifneq ($(HB_HAS_ZLIB_LOCAL),)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := unused
   include $(TOP)$(ROOT)config/none.mk
endif
