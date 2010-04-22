#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := png

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   png.c \
   pngerror.c \
   pngget.c \
   pngmem.c \
   pngpread.c \
   pngread.c \
   pngrio.c \
   pngrtran.c \
   pngrutil.c \
   pngset.c \
   pngtrans.c \
   pngwio.c \
   pngwrite.c \
   pngwtran.c \
   pngwutil.c \

ifeq ($(filter $(HB_COMPILER),poccarm xcc),)

   _DET_DSP_NAME := libpng
   _DET_VAR_INC_ := HB_INC_PNG
   _DET_VAR_HAS_ := HB_HAS_PNG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /boot/develop/headers/3rdparty
   _DET_INC_LOCL := $(realpath $(TOP)$(ROOT)external/png)
   _DET_INC_HEAD := /png.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_PNG_LOCAL),)

      ifneq ($(HB_HAS_ZLIB),)

         HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))

         ifeq ($(HB_COMPILER),djgpp)
            HB_CFLAGS += -DPNG_NO_SNPRINTF
         endif
         ifeq ($(HB_COMPILER),watcom)
            HB_CFLAGS += -DPNG_NO_SETJMP
         endif

         include $(TOP)$(ROOT)config/lib.mk
      else
         HB_SKIP_REASON := $(_DET_RES_TEXT)
         include $(TOP)$(ROOT)config/none.mk
      endif
   else
      HB_SKIP_REASON := unused
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
