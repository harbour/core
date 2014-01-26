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
   rt_tran2.prg \
   rt_trans.prg \
   rt_class.prg \

C_SOURCES := \
   rt_miscc.c \

PRG_MAIN := hbtest.prg

LIBS = $(HB_LIBS_ST_RDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   ifneq ($(HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk
