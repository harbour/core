#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no
HB_CFLAGS += -I$(TOP)$(ROOT)source/hbzlib

ifeq ($(HB_COMPILER),djgpp)
   HB_CFLAGS += -DPNG_NO_SNPRINTF
endif

LIBNAME := libpng

ifeq ($(HB_WITH_LIBPNG),)
   ifeq ($(HB_PLATFORM),win)
      HB_WITH_LIBPNG := yes
   endif
   ifeq ($(HB_PLATFORM),wce)
      HB_WITH_LIBPNG := yes
   endif
   ifeq ($(HB_PLATFORM),os2)
      HB_WITH_LIBPNG := yes
   endif
   ifeq ($(HB_PLATFORM),dos)
      HB_WITH_LIBPNG := yes
   endif
   ifeq ($(HB_PLATFORM),darwin)
      HB_WITH_LIBPNG := yes
   endif
   ifeq ($(HB_COMPILER),poccarm)
      HB_WITH_LIBPNG := no
   endif
endif

ifeq ($(HB_WITH_LIBPNG),yes)

C_SOURCES := \
   png.c \
   pngerror.c \
   pnggccrd.c \
   pngget.c \
   pngmem.c \
   pngpread.c \
   pngread.c \
   pngrio.c \
   pngrtran.c \
   pngrutil.c \
   pngset.c \
   pngtrans.c \
   pngvcrd.c \
   pngwio.c \
   pngwrite.c \
   pngwtran.c \
   pngwutil.c \

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
