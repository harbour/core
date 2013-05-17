
ROOT := ../../../

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

ifeq ($(filter $(HB_COMPILER),poccarm xcc tcc),)

   _DET_DSP_NAME := libpng
   _DET_VAR_INC_ := HB_INC_PNG
   _DET_VAR_HAS_ := HB_HAS_PNG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /boot/develop/headers/3rdparty
   _DET_INC_LOCL := src/3rd/png
   _DET_INC_HEAD := /png.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_PNG_LOCAL),)

      ifneq ($(HB_HAS_ZLIB),)

         HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))
         HB_CFLAGS_DYN := -DPNG_BUILD_DLL

         ifneq ($(filter $(HB_COMPILER),djgpp dmc),)
            HB_CFLAGS += -DPNG_NO_SNPRINTF
         endif
         ifneq ($(filter $(HB_COMPILER),pocc pocc64 poccarm),)
            HB_CFLAGS += -DPNG_ALLOCATED
         endif
         ifneq ($(filter $(HB_PLATFORM),wce),)
            HB_CFLAGS += -DPNG_NO_STDIO
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

# ORIGIN http://www.libpng.org/pub/png/libpng.html
# VER    1.6.2
# URL    http://prdownloads.sourceforge.net/libpng/libpng-1.6.2.tar.gz?download
# DIFF   png.dif
#
# MAP    LICENSE
# MAP    png.c
# MAP    pngconf.h
# MAP    scripts/pnglibconf.h.prebuilt           pnglconf.h
# MAP    pngerror.c
# MAP    pngget.c
# MAP    png.h
# MAP    pngmem.c
# MAP    pngpread.c
# MAP    pngpriv.h
# MAP    pngread.c
# MAP    pngrio.c
# MAP    pngrtran.c
# MAP    pngrutil.c
# MAP    pngset.c
# MAP    pngtrans.c
# MAP    pngwio.c
# MAP    pngwrite.c
# MAP    pngwtran.c
# MAP    pngwutil.c
# MAP    pnginfo.h
# MAP    pngstruct.h                             pngstru.h
# MAP    pngdebug.h
