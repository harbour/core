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
   hpdfndic.c \
   hpdfnull.c \
   hpdfnumb.c \
   hpdfobje.c \
   hpdfoutl.c \
   hpdfpage.c \
   hpdfpago.c \
   hpdfpags.c \
   hpdfpdfa.c \
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
# VER    2.2.0
# URL    http://libharu.org/files/libharu-2.2.0.tar.gz
# DIFF   libhpdf.dif
#
# MAP    README
# MAP    src/hpdf_annotation.c          hpdfanno.c
# MAP    src/hpdf_array.c               hpdfarra.c
# MAP    src/hpdf_binary.c              hpdfbina.c
# MAP    src/hpdf_boolean.c             hpdfbool.c
# MAP    src/hpdf_catalog.c             hpdfcata.c
# MAP    src/hpdf_destination.c         hpdfdest.c
# MAP    src/hpdf_dict.c                hpdfdict.c
# MAP    src/hpdf_doc.c                 hpdfdoc.c
# MAP    src/hpdf_doc_png.c             hpdfdocp.c
# MAP    src/hpdf_encoder.c             hpdfenco.c
# MAP    src/hpdf_encoder_cns.c         hpdfencc.c
# MAP    src/hpdf_encoder_cnt.c         hpdfencn.c
# MAP    src/hpdf_encoder_jp.c          hpdfencj.c
# MAP    src/hpdf_encoder_kr.c          hpdfenck.c
# MAP    src/hpdf_encrypt.c             hpdfecy.c
# MAP    src/hpdf_encryptdict.c         hpdfecyd.c
# MAP    src/hpdf_error.c               hpdferro.c
# MAP    src/hpdf_ext_gstate.c          hpdfextg.c
# MAP    src/hpdf_font.c                hpdffont.c
# MAP    src/hpdf_font_cid.c            hpdffonc.c
# MAP    src/hpdf_font_tt.c             hpdffott.c
# MAP    src/hpdf_font_type1.c          hpdffon1.c
# MAP    src/hpdf_fontdef.c             hpdffdf.c
# MAP    src/hpdf_fontdef_base14.c      hpdffdfb.c
# MAP    src/hpdf_fontdef_cid.c         hpdffdfi.c
# MAP    src/hpdf_fontdef_cns.c         hpdffdfc.c
# MAP    src/hpdf_fontdef_cnt.c         hpdffdfn.c
# MAP    src/hpdf_fontdef_jp.c          hpdffdfj.c
# MAP    src/hpdf_fontdef_kr.c          hpdffdfk.c
# MAP    src/hpdf_fontdef_tt.c          hpdffdft.c
# MAP    src/hpdf_fontdef_type1.c       hpdffdf1.c
# MAP    src/hpdf_gstate.c              hpdfgsta.c
# MAP    src/hpdf_image.c               hpdfimag.c
# MAP    src/hpdf_image_png.c           hpdfimap.c
# MAP    src/hpdf_info.c                hpdfinfo.c
# MAP    src/hpdf_list.c                hpdflist.c
# MAP    src/hpdf_mmgr.c                hpdfmmgr.c
# MAP    src/hpdf_name.c                hpdfname.c
# MAP    src/hpdf_namedict.c            hpdfndic.c
# MAP    src/hpdf_null.c                hpdfnull.c
# MAP    src/hpdf_number.c              hpdfnumb.c
# MAP    src/hpdf_objects.c             hpdfobje.c
# MAP    src/hpdf_outline.c             hpdfoutl.c
# MAP    src/hpdf_pdfa.c                hpdfpdfa.c
# MAP    src/hpdf_page_label.c          hpdfpage.c
# MAP    src/hpdf_page_operator.c       hpdfpago.c
# MAP    src/hpdf_pages.c               hpdfpags.c
# MAP    src/hpdf_real.c                hpdfreal.c
# MAP    src/hpdf_streams.c             hpdfstre.c
# MAP    src/hpdf_string.c              hpdfstri.c
# MAP    src/hpdf_u3d.c                 hpdfu3d.c
# MAP    src/hpdf_utils.c               hpdfutil.c
# MAP    src/hpdf_xref.c                hpdfxref.c
# MAP    include/hpdf.h                 hpdf.h
# MAP    include/hpdf_annotation.h      hpdfanno.h
# MAP    include/hpdf_catalog.h         hpdfcata.h
# MAP    include/hpdf_conf.h            hpdfconf.h
# MAP    include/hpdf_consts.h          hpdfcons.h
# MAP    include/hpdf_destination.h     hpdfdest.h
# MAP    include/hpdf_doc.h             hpdfdoc.h
# MAP    include/hpdf_encoder.h         hpdfenco.h
# MAP    include/hpdf_encrypt.h         hpdfencr.h
# MAP    include/hpdf_encryptdict.h     hpdfency.h
# MAP    include/hpdf_error.h           hpdferro.h
# MAP    include/hpdf_ext_gstate.h      hpdfextg.h
# MAP    include/hpdf_font.h            hpdffont.h
# MAP    include/hpdf_fontdef.h         hpdffond.h
# MAP    include/hpdf_gstate.h          hpdfgsta.h
# MAP    include/hpdf_image.h           hpdfimag.h
# MAP    include/hpdf_info.h            hpdfinfo.h
# MAP    include/hpdf_list.h            hpdflist.h
# MAP    include/hpdf_mmgr.h            hpdfmmgr.h
# MAP    include/hpdf_namedict.h        hpdfndic.h
# MAP    include/hpdf_objects.h         hpdfobje.h
# MAP    include/hpdf_outline.h         hpdfoutl.h
# MAP    include/hpdf_pdfa.h            hpdfpdfa.h
# MAP    include/hpdf_page_label.h      hpdfpage.h
# MAP    include/hpdf_pages.h           hpdfpags.h
# MAP    include/hpdf_streams.h         hpdfstre.h
# MAP    include/hpdf_types.h           hpdftype.h
# MAP    include/hpdf_u3d.h             hpdfu3d.h
# MAP    include/hpdf_utils.h           hpdfutil.h
# MAP    include/hpdf_version.h         hpdfvers.h
