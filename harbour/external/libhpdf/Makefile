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
   _DET_INC_LOCL := external/libhpdf
   _DET_INC_HEAD := /hpdf.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_LIBHARU_LOCAL),)

      _DET_DSP_NAME := libpng
      _DET_VAR_INC_ := HB_INC_PNG
      _DET_VAR_HAS_ := HB_HAS_PNG
      _DET_FLT_PLAT :=
      _DET_FLT_COMP :=
      _DET_INC_DEFP := /usr/include /opt/local/include
      _DET_INC_LOCL := external/png
      _DET_INC_HEAD := /png.h
      include $(TOP)$(ROOT)config/detfun.mk

      ifneq ($(HB_HAS_PNG),)

         ifneq ($(HB_HAS_ZLIB),)

            HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))
            HB_CFLAGS += $(foreach d,$(HB_HAS_PNG),-I$(d))
            # Suppress deprecated warnings until libharu gets updated for latest libpng
            HB_CFLAGS += -DPNG_NO_PEDANTIC_WARNINGS
            HB_CFLAGS_DYN := -DHPDF_DLL_MAKE

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

# ORIGIN http://libharu.org/
# VER    2.1.0
# URL    http://libharu.org/files/libharu-2.1.0.tar.gz
# DIFF   libhpdf.dif
#
# MAP    README
# MAP    hpdf_annotation.c      hpdfanno.c
# MAP    hpdf_array.c           hpdfarra.c
# MAP    hpdf_binary.c          hpdfbina.c
# MAP    hpdf_boolean.c         hpdfbool.c
# MAP    hpdf_catalog.c         hpdfcata.c
# MAP    hpdf_destination.c     hpdfdest.c
# MAP    hpdf_dict.c            hpdfdict.c
# MAP    hpdf_doc.c             hpdfdoc.c
# MAP    hpdf_doc_png.c         hpdfdocp.c
# MAP    hpdf_encoder.c         hpdfenco.c
# MAP    hpdf_encoder_cns.c     hpdfencc.c
# MAP    hpdf_encoder_cnt.c     hpdfencn.c
# MAP    hpdf_encoder_jp.c      hpdfencj.c
# MAP    hpdf_encoder_kr.c      hpdfenck.c
# MAP    hpdf_encrypt.c         hpdfecy.c
# MAP    hpdf_encryptdict.c     hpdfecyd.c
# MAP    hpdf_error.c           hpdferro.c
# MAP    hpdf_ext_gstate.c      hpdfextg.c
# MAP    hpdf_font.c            hpdffont.c
# MAP    hpdf_font_cid.c        hpdffonc.c
# MAP    hpdf_font_tt.c         hpdffott.c
# MAP    hpdf_font_type1.c      hpdffon1.c
# MAP    hpdf_fontdef.c         hpdffdf.c
# MAP    hpdf_fontdef_base14.c  hpdffdfb.c
# MAP    hpdf_fontdef_cid.c     hpdffdfi.c
# MAP    hpdf_fontdef_cns.c     hpdffdfc.c
# MAP    hpdf_fontdef_cnt.c     hpdffdfn.c
# MAP    hpdf_fontdef_jp.c      hpdffdfj.c
# MAP    hpdf_fontdef_kr.c      hpdffdfk.c
# MAP    hpdf_fontdef_tt.c      hpdffdft.c
# MAP    hpdf_fontdef_type1.c   hpdffdf1.c
# MAP    hpdf_gstate.c          hpdfgsta.c
# MAP    hpdf_image.c           hpdfimag.c
# MAP    hpdf_image_png.c       hpdfimap.c
# MAP    hpdf_info.c            hpdfinfo.c
# MAP    hpdf_list.c            hpdflist.c
# MAP    hpdf_mmgr.c            hpdfmmgr.c
# MAP    hpdf_name.c            hpdfname.c
# MAP    hpdf_null.c            hpdfnull.c
# MAP    hpdf_number.c          hpdfnumb.c
# MAP    hpdf_objects.c         hpdfobje.c
# MAP    hpdf_outline.c         hpdfoutl.c
# MAP    hpdf_page_label.c      hpdfpage.c
# MAP    hpdf_page_operator.c   hpdfpago.c
# MAP    hpdf_pages.c           hpdfpags.c
# MAP    hpdf_real.c            hpdfreal.c
# MAP    hpdf_streams.c         hpdfstre.c
# MAP    hpdf_string.c          hpdfstri.c
# MAP    hpdf_u3d.c             hpdfu3d.c
# MAP    hpdf_utils.c           hpdfutil.c
# MAP    hpdf_xref.c            hpdfxref.c
# MAP    hpdf.h
# MAP    hpdf_annotation.h      hpdfanno.h
# MAP    hpdf_catalog.h         hpdfcata.h
# MAP    hpdf_conf.h            hpdfconf.h
# MAP    hpdf_config.h          hpdfcfg.h
# MAP    hpdf_consts.h          hpdfcons.h
# MAP    hpdf_destination.h     hpdfdest.h
# MAP    hpdf_doc.h             hpdfdoc.h
# MAP    hpdf_encoder.h         hpdfenco.h
# MAP    hpdf_encrypt.h         hpdfencr.h
# MAP    hpdf_encryptdict.h     hpdfency.h
# MAP    hpdf_error.h           hpdferro.h
# MAP    hpdf_ext_gstate.h      hpdfextg.h
# MAP    hpdf_font.h            hpdffont.h
# MAP    hpdf_fontdef.h         hpdffond.h
# MAP    hpdf_gstate.h          hpdfgsta.h
# MAP    hpdf_image.h           hpdfimag.h
# MAP    hpdf_info.h            hpdfinfo.h
# MAP    hpdf_list.h            hpdflist.h
# MAP    hpdf_mmgr.h            hpdfmmgr.h
# MAP    hpdf_objects.h         hpdfobje.h
# MAP    hpdf_outline.h         hpdfoutl.h
# MAP    hpdf_page_label.h      hpdfpage.h
# MAP    hpdf_pages.h           hpdfpags.h
# MAP    hpdf_streams.h         hpdfstre.h
# MAP    hpdf_types.h           hpdftype.h
# MAP    hpdf_u3d.h             hpdfu3d.h
# MAP    hpdf_utils.h           hpdfutil.h
# MAP    hpdf_version.h         hpdfvers.h
