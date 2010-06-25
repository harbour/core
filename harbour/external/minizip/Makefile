#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := minizip

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   ioapi.c \
   zip.c \
   unzip.c \

ifneq ($(HB_HAS_ZLIB),)
   HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN http://www.winimage.com/zLibDll/minizip.html
# VER    1.1
# URL    http://www.winimage.com/zLibDll/unzip11.zip
# DIFF   minizip.dif
#
# MAP    crypt.h
# MAP    ioapi.c
# MAP    ioapi.h
# MAP    unzip.c
# MAP    unzip.h
# MAP    zip.c
# MAP    zip.h
