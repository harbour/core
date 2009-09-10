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

   # force redetection
   HB_HAS_ZLIB :=

   _DET_DSP_NAME := zlib
   _DET_VAR_INC_ := HB_INC_ZLIB
   _DET_VAR_HAS_ := HB_HAS_ZLIB
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include
   ifneq ($(filter $(HB_PLATFORM),win wce os2 dos),)
      _DET_INC_DEFP += $(TOP)$(ROOT)external/zlib
   endif
   _DET_INC_HEAD := /zlib.h

   include $(TOP)$(ROOT)config/detfun.mk

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
   include $(TOP)$(ROOT)config/none.mk
endif
