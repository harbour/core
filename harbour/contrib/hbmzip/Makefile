#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbmzip

C_SOURCES := \
   ioapi.c \
   zip.c \
   unzip.c \
   hbmzip.c

PRG_HEADERS := \
   hbmzip.ch

ifneq ($(HB_HAS_ZLIB),)
   HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
