#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no
HB_BUILD_MODE := c

LIBNAME := libhpdf

C_SOURCES := \
   _hbhbpdf.c \
   hpdfanno.c \
   hpdfarra.c \
   hpdfbina.c \
   hpdfbool.c \
   hpdfcata.c \
   hpdfdest.c \
   hpdfdict.c \
   hpdfdoc.c \
   hpdfdocp.c \
   hpdfecy.c \
   hpdfecyd.c \
   hpdfencc.c \
   hpdfencj.c \
   hpdfenck.c \
   hpdfencn.c \
   hpdfenco.c \
   hpdferro.c \
   hpdfextg.c \
   hpdffdf.c \
   hpdffdf1.c \
   hpdffdfb.c \
   hpdffdfc.c \
   hpdffdfi.c \
   hpdffdfj.c \
   hpdffdfk.c \
   hpdffdfn.c \
   hpdffdft.c \
   hpdffon1.c \
   hpdffonc.c \
   hpdffont.c \
   hpdffott.c \
   hpdfgsta.c \
   hpdfimag.c \
   hpdfimap.c \
   hpdfinfo.c \
   hpdflist.c \
   hpdfmmgr.c \
   hpdfname.c \
   hpdfnull.c \
   hpdfnumb.c \
   hpdfobje.c \
   hpdfoutl.c \
   hpdfpage.c \
   hpdfpago.c \
   hpdfpags.c \
   hpdfreal.c \
   hpdfstre.c \
   hpdfstri.c \
   hpdfu3d.c \
   hpdfutil.c \
   hpdfxref.c \

# decide whether to build it locally
ifeq ($(HB_INC_LIBHARU),)
   HB_INC_LIBHARU := yes
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),win-watcom)
      HB_INC_LIBHARU := no
   endif
   ifeq ($(HB_COMPILER),pocc64)
      HB_INC_LIBHARU := no
   endif
   ifeq ($(HB_PLATFORM),wce)
      HB_INC_LIBHARU := no
   endif
   ifeq ($(HB_COMPILER),mingwarm)
      HB_INC_LIBHARU := yes
   endif
endif

ifneq ($(HB_INC_LIBHARU),no)

   _DET_DSP_NAME := libpng
   _DET_VAR_INC_ := HB_INC_PNG
   _DET_VAR_HAS_ := HB_HAS_PNG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include
   ifneq ($(filter $(HB_PLATFORM),win wce os2 dos darwin),)
      _DET_INC_DEFP += $(TOP)$(ROOT)external/libpng
   endif
   _DET_INC_HEAD := /png.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_PNG),)

      HB_CFLAGS += -I$(TOP)$(ROOT)source/hbzlib
      HB_CFLAGS += $(foreach d,$(HB_HAS_PNG),-I$(d))

      ifeq ($(HB_COMPILER),mingwarm)
         HB_CFLAGS += -U__COREDLL__
      endif

      include $(TOP)$(ROOT)config/header.mk
      include $(TOP)$(ROOT)config/lib.mk

   else
      HB_SKIP_REASON := $(_DET_RES_TEXT)
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   include $(TOP)$(ROOT)config/none.mk
endif
