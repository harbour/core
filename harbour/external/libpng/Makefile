#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := libpng

HB_BUILD_WARN := no

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

# decide if it's supported at all
HB_SUPPORTED := yes
ifeq ($(HB_COMPILER),poccarm)
   HB_SUPPORTED := no
endif
ifeq ($(HB_COMPILER),xcc)
   HB_SUPPORTED := no
endif

ifeq ($(HB_SUPPORTED),yes)

   _DET_DSP_NAME := libpng
   _DET_VAR_INC_ := HB_INC_PNG
   _DET_VAR_HAS_ := HB_HAS_PNG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /boot/develop/headers/3rdparty
   _DET_INC_LOCL := $(TOP)$(ROOT)external/libpng
   _DET_INC_HEAD := /png.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_PNG_LOCAL),)

      ifneq ($(HB_HAS_ZLIB),)

         HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))

         ifeq ($(HB_COMPILER),djgpp)
            HB_CFLAGS += -DPNG_NO_SNPRINTF
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
   HB_SKIP_REASON := platform or compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
