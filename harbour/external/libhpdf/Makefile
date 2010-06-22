#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no
HB_BUILD_MODE := c

LIBNAME := libhpdf

# URL (stable): http://libharu.org/files/libharu-2.1.0.tar.gz
# URL (unstable): git clone http://git.libharu.org/libharu.git

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

# Use with 'hbrun <root>/config/ren_sfn.prg [F|T]'
#
# RENAME README     README
# RENAME hpdfanno.c hpdf_annotation.c
# RENAME hpdfarra.c hpdf_array.c
# RENAME hpdfbina.c hpdf_binary.c
# RENAME hpdfbool.c hpdf_boolean.c
# RENAME hpdfcata.c hpdf_catalog.c
# RENAME hpdfdest.c hpdf_destination.c
# RENAME hpdfdict.c hpdf_dict.c
# RENAME hpdfdoc.c  hpdf_doc.c
# RENAME hpdfdocp.c hpdf_doc_png.c
# RENAME hpdfenco.c hpdf_encoder.c
# RENAME hpdfencc.c hpdf_encoder_cns.c
# RENAME hpdfencn.c hpdf_encoder_cnt.c
# RENAME hpdfencj.c hpdf_encoder_jp.c
# RENAME hpdfenck.c hpdf_encoder_kr.c
# RENAME hpdfecy.c  hpdf_encrypt.c
# RENAME hpdfecyd.c hpdf_encryptdict.c
# RENAME hpdferro.c hpdf_error.c
# RENAME hpdfextg.c hpdf_ext_gstate.c
# RENAME hpdffont.c hpdf_font.c
# RENAME hpdffonc.c hpdf_font_cid.c
# RENAME hpdffott.c hpdf_font_tt.c
# RENAME hpdffon1.c hpdf_font_type1.c
# RENAME hpdffdf.c  hpdf_fontdef.c
# RENAME hpdffdfb.c hpdf_fontdef_base14.c
# RENAME hpdffdfi.c hpdf_fontdef_cid.c
# RENAME hpdffdfc.c hpdf_fontdef_cns.c
# RENAME hpdffdfn.c hpdf_fontdef_cnt.c
# RENAME hpdffdfj.c hpdf_fontdef_jp.c
# RENAME hpdffdfk.c hpdf_fontdef_kr.c
# RENAME hpdffdft.c hpdf_fontdef_tt.c
# RENAME hpdffdf1.c hpdf_fontdef_type1.c
# RENAME hpdfgsta.c hpdf_gstate.c
# RENAME hpdfimag.c hpdf_image.c
# RENAME hpdfimap.c hpdf_image_png.c
# RENAME hpdfinfo.c hpdf_info.c
# RENAME hpdflist.c hpdf_list.c
# RENAME hpdfmmgr.c hpdf_mmgr.c
# RENAME hpdfname.c hpdf_name.c
# RENAME hpdfnull.c hpdf_null.c
# RENAME hpdfnumb.c hpdf_number.c
# RENAME hpdfobje.c hpdf_objects.c
# RENAME hpdfoutl.c hpdf_outline.c
# RENAME hpdfpage.c hpdf_page_label.c
# RENAME hpdfpago.c hpdf_page_operator.c
# RENAME hpdfpags.c hpdf_pages.c
# RENAME hpdfreal.c hpdf_real.c
# RENAME hpdfstre.c hpdf_streams.c
# RENAME hpdfstri.c hpdf_string.c
# RENAME hpdfu3d.c  hpdf_u3d.c
# RENAME hpdfutil.c hpdf_utils.c
# RENAME hpdfxref.c hpdf_xref.c
# RENAME hpdf.h     hpdf.h
# RENAME hpdfanno.h hpdf_annotation.h
# RENAME hpdfcata.h hpdf_catalog.h
# RENAME hpdfconf.h hpdf_conf.h
# RENAME hpdfcfg.h  hpdf_config.h
# RENAME hpdfcons.h hpdf_consts.h
# RENAME hpdfdest.h hpdf_destination.h
# RENAME hpdfdoc.h  hpdf_doc.h
# RENAME hpdfenco.h hpdf_encoder.h
# RENAME hpdfencr.h hpdf_encrypt.h
# RENAME hpdfency.h hpdf_encryptdict.h
# RENAME hpdferro.h hpdf_error.h
# RENAME hpdfextg.h hpdf_ext_gstate.h
# RENAME hpdffont.h hpdf_font.h
# RENAME hpdffond.h hpdf_fontdef.h
# RENAME hpdfgsta.h hpdf_gstate.h
# RENAME hpdfimag.h hpdf_image.h
# RENAME hpdfinfo.h hpdf_info.h
# RENAME hpdflist.h hpdf_list.h
# RENAME hpdfmmgr.h hpdf_mmgr.h
# RENAME hpdfobje.h hpdf_objects.h
# RENAME hpdfoutl.h hpdf_outline.h
# RENAME hpdfpage.h hpdf_page_label.h
# RENAME hpdfpags.h hpdf_pages.h
# RENAME hpdfstre.h hpdf_streams.h
# RENAME hpdftype.h hpdf_types.h
# RENAME hpdfu3d.h  hpdf_u3d.h
# RENAME hpdfutil.h hpdf_utils.h
# RENAME hpdfvers.h hpdf_version.h
