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
   hpdf_annotation.c \
   hpdf_array.c \
   hpdf_binary.c \
   hpdf_boolean.c \
   hpdf_catalog.c \
   hpdf_destination.c \
   hpdf_dict.c \
   hpdf_doc.c \
   hpdf_doc_png.c \
   hpdf_encoder.c \
   hpdf_encoder_cns.c \
   hpdf_encoder_cnt.c \
   hpdf_encoder_jp.c \
   hpdf_encoder_kr.c \
   hpdf_encrypt.c \
   hpdf_encryptdict.c \
   hpdf_error.c \
   hpdf_ext_gstate.c \
   hpdf_font.c \
   hpdf_font_cid.c \
   hpdf_font_tt.c \
   hpdf_font_type1.c \
   hpdf_fontdef.c \
   hpdf_fontdef_base14.c \
   hpdf_fontdef_cid.c \
   hpdf_fontdef_cns.c \
   hpdf_fontdef_cnt.c \
   hpdf_fontdef_jp.c \
   hpdf_fontdef_kr.c \
   hpdf_fontdef_tt.c \
   hpdf_fontdef_type1.c \
   hpdf_gstate.c \
   hpdf_image.c \
   hpdf_image_png.c \
   hpdf_info.c \
   hpdf_list.c \
   hpdf_mmgr.c \
   hpdf_name.c \
   hpdf_null.c \
   hpdf_number.c \
   hpdf_objects.c \
   hpdf_outline.c \
   hpdf_page_label.c \
   hpdf_page_operator.c \
   hpdf_pages.c \
   hpdf_real.c \
   hpdf_streams.c \
   hpdf_string.c \
   hpdf_u3d.c \
   hpdf_utils.c \
   hpdf_xref.c \

# decide whether to build it locally
ifeq ($(HB_INC_LIBHARU),)
   HB_INC_LIBHARU := yes
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),win-watcom)
      HB_INC_LIBHARU := no
   endif
   # NOTE: dos hosts can't build this lib due to long filename. [vszakats]
   ifeq ($(HB_HOST_PLAT),dos)
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
      HB_INC_LIBPNG += $(TOP)$(ROOT)external/libpng
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
      INSTALL_RULE_HEADERS := $(INSTALL_RULE)
      include $(TOP)$(ROOT)config/lib.mk

      install::
	$(INSTALL_RULE_HEADERS)

   else
      HB_SKIP_REASON := $(_DET_RES_TEXT)
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   include $(TOP)$(ROOT)config/none.mk
endif
