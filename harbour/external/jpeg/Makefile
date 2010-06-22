#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := jpeg

# URL: http://www.ijg.org/files/jpegsr8b.zip

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   jaricom.c \
   jcapimin.c \
   jcapistd.c \
   jcarith.c \
   jccoefct.c \
   jccolor.c \
   jcdctmgr.c \
   jcinit.c \
   jcmainct.c \
   jcmarker.c \
   jcmaster.c \
   jcomapi.c \
   jcparam.c \
   jcprepct.c \
   jcsample.c \
   jctrans.c \
   jdapimin.c \
   jdapistd.c \
   jdarith.c \
   jdatadst.c \
   jdatasrc.c \
   jdcoefct.c \
   jdcolor.c \
   jddctmgr.c \
   jdhuff.c \
   jdinput.c \
   jdmainct.c \
   jdmarker.c \
   jdmaster.c \
   jdmerge.c \
   jdpostct.c \
   jdsample.c \
   jdtrans.c \
   jerror.c \
   jfdctflt.c \
   jfdctfst.c \
   jfdctint.c \
   jidctflt.c \
   jidctfst.c \
   jidctint.c \
   jmemmgr.c \
   jmemnobs.c \
   jquant1.c \
   jquant2.c \
   jutils.c \
   rdbmp.c \
   rdcolmap.c \
   rdgif.c \
   rdppm.c \
   rdrle.c \
   rdswitch.c \
   rdtarga.c \
   transupp.c \
   wrbmp.c \
   wrgif.c \
   wrppm.c \
   wrrle.c \
   wrtarga.c \

#   jmemansi.c \
#   jmemdos.c \
#   jmemname.c \
#   jmemmac.c \

ifeq ($(filter $(HB_COMPILER),<>),)

   _DET_DSP_NAME := jpeg
   _DET_VAR_INC_ := HB_INC_JPEG
   _DET_VAR_HAS_ := HB_HAS_JPEG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include
   _DET_INC_LOCL := external/jpeg
   _DET_INC_HEAD := /jpeglib.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_JPEG_LOCAL),)
      HB_CFLAGS += -DHAVE_STDLIB_H -DHAVE_PROTOTYPES
      ifeq ($(HB_COMPILER),poccarm)
         HB_CFLAGS += -DNO_GETENV
      endif

      include $(TOP)$(ROOT)config/lib.mk
   else
      HB_SKIP_REASON := unused
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
