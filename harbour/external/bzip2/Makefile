#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := bz2

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   blocksor.c \
   bzlib.c \
   compress.c \
   crctable.c \
   decompre.c \
   huffman.c \
   randtabl.c \

_DET_DSP_NAME := bzip2
_DET_VAR_INC_ := HB_INC_BZIP2
_DET_VAR_HAS_ := HB_HAS_BZIP2
_DET_FLT_PLAT := !wce
_DET_FLT_COMP := !pocc64
_DET_INC_DEFP := /usr/include
_DET_INC_LOCL := external/bzip2
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

# ORIGIN http://www.bzip.org/
# VER    1.0.6
# URL    http://www.bzip.org/1.0.6/bzip2-1.0.6.tar.gz
# DIFF
#
# MAP    LICENSE
# MAP    blocksort.c            blocksor.c
# MAP    bzlib.c
# MAP    bzlib.h
# MAP    bzlib_private.h        bzlib_pr.h
# MAP    compress.c
# MAP    crctable.c
# MAP    decompress.c           decompre.c
# MAP    huffman.c
# MAP    randtable.c            randtabl.c
