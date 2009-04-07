#
# $Id$
#

HB_BUILD_WARN = no
HB_BUILD_MODE = c
HB_USER_CFLAGS += -I$(TOP)$(ROOT)source/hbzlib

ROOT = ../../

LIBNAME=libhpdf

ifeq ($(HB_WITH_LIBHPDF),)
HB_WITH_LIBHPDF=yes
ifeq ($(HB_COMPILER),owatcom)
HB_WITH_LIBHPDF=no
endif
ifeq ($(HB_COMPILER),pocc64)
HB_WITH_LIBHPDF=no
endif
ifeq ($(HB_COMPILER),poccce)
HB_WITH_LIBHPDF=no
endif
ifeq ($(HB_COMPILER),mingwce)
HB_WITH_LIBHPDF=no
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_LIBHPDF=no
endif
endif

ifeq ($(HB_WITH_LIBHPDF),yes)

ifeq ($(HB_INC_LIBPNG),)
ifeq ($(HB_XBUILD),)
HB_INC_LIBPNG = /usr/include
ifeq ($(HB_ARCHITECTURE),win)
HB_INC_LIBPNG += $(TOP)$(ROOT)external/libpng
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_INC_LIBPNG += $(TOP)$(ROOT)external/libpng
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_INC_LIBPNG += $(TOP)$(ROOT)external/libpng
endif
ifeq ($(HB_ARCHITECTURE),dos)
HB_INC_LIBPNG += $(TOP)$(ROOT)external/libpng
endif
endif
endif

HB_INC_LIBPNG_OK += $(foreach d, $(HB_INC_LIBPNG), $(if $(wildcard $(d)/png.h),$(d),))

ifneq ($(strip $(HB_INC_LIBPNG_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_LIBPNG_OK), -I$(d))

C_SOURCES = \
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

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
