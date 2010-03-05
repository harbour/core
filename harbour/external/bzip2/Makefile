#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := bzip2

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   blocksor.c \
   bzip2.c \
   bzlib.c \
   compress.c \
   crctable.c \
   decompre.c \
   huffman.c \
   randtabl.c \

_DET_DSP_NAME := bzip2
_DET_VAR_INC_ := HB_INC_BZIP2
_DET_VAR_HAS_ := HB_HAS_BZIP2
_DET_FLT_PLAT := !dos !os2 !wce
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
_DET_INC_LOCL := $(realpath $(TOP)$(ROOT)external/bzip2)
_DET_INC_HEAD := /bzlib.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_BZIP2_LOCAL),)
   ifeq ($(HB_COMPILER),bcc)
      HB_AFLAGS += /C
   endif
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := unused
   include $(TOP)$(ROOT)config/none.mk
endif
