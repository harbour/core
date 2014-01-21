ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := jpeg

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
   _DET_INC_LOCL := src/3rd/jpeg
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

# ORIGIN http://www.ijg.org/
# VER    9
# URL    http://www.ijg.org/files/jpegsrc.v9.tar.gz
# DIFF   jpeg.dif
#
# MAP    README
# MAP    cderror.h
# MAP    cdjpeg.h
# MAP    jaricom.c
# MAP    jcapimin.c
# MAP    jcapistd.c
# MAP    jcarith.c
# MAP    jccoefct.c
# MAP    jccolor.c
# MAP    jcdctmgr.c
# MAP    jchuff.c
# MAP    jcinit.c
# MAP    jcmainct.c
# MAP    jcmarker.c
# MAP    jcmaster.c
# MAP    jcomapi.c
# MAP    jcparam.c
# MAP    jcprepct.c
# MAP    jcsample.c
# MAP    jctrans.c
# MAP    jdapimin.c
# MAP    jdapistd.c
# MAP    jdarith.c
# MAP    jdatadst.c
# MAP    jdatasrc.c
# MAP    jdcoefct.c
# MAP    jdcolor.c
# MAP    jdct.h
# MAP    jddctmgr.c
# MAP    jdhuff.c
# MAP    jdinput.c
# MAP    jdmainct.c
# MAP    jdmarker.c
# MAP    jdmaster.c
# MAP    jdmerge.c
# MAP    jdpostct.c
# MAP    jdsample.c
# MAP    jdtrans.c
# MAP    jerror.c
# MAP    jerror.h
# MAP    jfdctflt.c
# MAP    jfdctfst.c
# MAP    jfdctint.c
# MAP    jidctflt.c
# MAP    jidctfst.c
# MAP    jidctint.c
# MAP    jinclude.h
# MAP    jmemansi.c
# MAP    jmemdos.c
# MAP    jmemmac.c
# MAP    jmemmgr.c
# MAP    jmemname.c
# MAP    jmemnobs.c
# MAP    jmemsys.h
# MAP    jmorecfg.h
# MAP    jpegint.h
# MAP    jpeglib.h
# MAP    jquant1.c
# MAP    jquant2.c
# MAP    jutils.c
# MAP    jversion.h
# MAP    rdbmp.c
# MAP    rdcolmap.c
# MAP    rdgif.c
# MAP    rdppm.c
# MAP    rdrle.c
# MAP    rdswitch.c
# MAP    rdtarga.c
# MAP    transupp.c
# MAP    transupp.h
# MAP    wrbmp.c
# MAP    wrgif.c
# MAP    wrppm.c
# MAP    wrrle.c
# MAP    wrtarga.c
