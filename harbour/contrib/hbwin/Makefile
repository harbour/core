#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbwin

C_SOURCES := \
   mapi.c \
   olecore.c \
   oleinit.c \
   axcore.c \
   legacycd.c \
   legacyco.c \
   legacycp.c \
   legacycr.c \
   legacycv.c \
   wce_simc.c \
   wce_smsc.c \
   win_bmp.c \
   win_bmpd.c \
   win_com.c \
   win_dlg.c \
   win_dll.c \
   win_misc.c \
   win_osc.c \
   win_prn1.c \
   win_prn2.c \
   win_prn3.c \
   win_regc.c \
   wapi_alloc.c \
   wapi_commctrl.c \
   wapi_err.c \
   wapi_shellapi.c \
   wapi_winbase.c \
   wapi_winbase_mutex.c \
   wapi_wingdi.c \
   wapi_wingdi_font.c \
   wapi_winnls.c \
   wapi_winuser.c \

PRG_SOURCES := \
   legacy.prg \
   oleauto.prg \
   axfunc.prg \
   wce_sim.prg \
   win_os.prg \
   win_reg.prg \
   win_tbmp.prg \
   win_tcom.prg \
   win_tprn.prg \

C_HEADERS := \
   hbwapi.h \
   hbwin.h \
   hbwinole.h \

PRG_HEADERS := \
   hbwin.ch \

ifneq ($(filter $(HB_PLATFORM),win wce),)

   _DET_DSP_NAME := libpng
   _DET_VAR_INC_ := HB_INC_PNG
   _DET_VAR_HAS_ := HB_HAS_PNG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP :=
   _DET_INC_LOCL := $(realpath $(TOP)$(ROOT)external/libpng)
   _DET_INC_HEAD := /png.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_PNG),)
      HB_CFLAGS += -DHB_HAS_PNG $(foreach d,$(HB_HAS_PNG),-I$(d))

      ifneq ($(HB_HAS_ZLIB),)
         HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))
      endif
   endif

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
