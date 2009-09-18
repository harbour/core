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

# decide if it's supported at all
HB_SUPPORTED := yes
ifeq ($(HB_PLATFORM)-$(HB_COMPILER),win-watcom)
   HB_SUPPORTED := no
endif
ifeq ($(HB_COMPILER),pocc64)
   HB_SUPPORTED := no
endif
ifeq ($(HB_PLATFORM),wce)
   HB_SUPPORTED := no
endif
ifeq ($(HB_COMPILER),mingwarm)
   HB_SUPPORTED := yes
endif

ifeq ($(HB_SUPPORTED),yes)

   _DET_DSP_NAME := libharu
   _DET_VAR_INC_ := HB_INC_LIBHARU
   _DET_VAR_HAS_ := HB_HAS_LIBHARU
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /usr/local/include
   _DET_INC_LOCL := $(realpath $(TOP)$(ROOT)external/libhpdf)
   _DET_INC_HEAD := /hpdf.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_LIBHARU_LOCAL),)

      _DET_DSP_NAME := libpng
      _DET_VAR_INC_ := HB_INC_PNG
      _DET_VAR_HAS_ := HB_HAS_PNG
      _DET_FLT_PLAT :=
      _DET_FLT_COMP :=
      _DET_INC_DEFP := /usr/include /opt/local/include
      _DET_INC_LOCL := $(realpath $(TOP)$(ROOT)external/libpng)
      _DET_INC_HEAD := /png.h
      include $(TOP)$(ROOT)config/detfun.mk

      ifneq ($(HB_HAS_PNG),)

         ifneq ($(HB_HAS_ZLIB),)

            HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))
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
