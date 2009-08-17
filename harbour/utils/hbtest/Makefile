#
# $Id$
#

ifeq ($(HB_MAIN),)
   HB_MAIN := std
endif

ROOT := ../../

PRG_SOURCES := \
   hbtest.prg \
   rt_array.prg \
   rt_date.prg \
   rt_file.prg \
   rt_hvm.prg \
   rt_hvma.prg \
   rt_math.prg \
   rt_misc.prg \
   rt_mt.prg \
   rt_str.prg \
   rt_stra.prg \
   rt_trans.prg \
   rt_class.prg \

C_SOURCES := \
   rt_miscc.c \

PRG_MAIN := hbtest.prg

LIBS = $(HB_LIBS_ST_RDD)

include $(TOP)$(ROOT)config/bin.mk
