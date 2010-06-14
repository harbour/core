#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sddoci

C_SOURCES := \
   sddoci.c \

_DET_DSP_NAME := ocilib
_DET_VAR_INC_ := HB_INC_OCILIB
_DET_VAR_HAS_ := HB_HAS_OCILIB
_DET_FLT_PLAT := !dos !os2 !wce
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/include
_DET_INC_HEAD := /ocilib.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_OCILIB),)
   HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/rddsql
   HB_CFLAGS += $(foreach d,$(HB_HAS_OCILIB),-I$(d))
   # Using prebuilt static libs with MinGW
   ifeq ($(HB_COMPILER),mingw)
      HB_CFLAGS += -DOCI_API=_stdcall
   endif
   # Required to link against generated implib
   ifeq ($(HB_COMPILER),bcc)
      HB_CFLAGS += -DOCI_API=_stdcall
   endif
   ifneq ($(HB_WITH_OCILIB_WIDE),)
      HB_CFLAGS += -DOCI_CHARSET_UNICODE -DOCI_CHARSET_WIDE
   else
   ifneq ($(HB_WITH_OCILIB_MIXED),)
      HB_CFLAGS += -DOCI_CHARSET_MIXED
   endif
   endif
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
