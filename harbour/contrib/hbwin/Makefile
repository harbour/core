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
   win_com.c \
   win_dll.c \
   win_misc.c \
   win_osc.c \
   win_prn1.c \
   win_prn2.c \
   win_prn3.c \
   win_regc.c \
   wapi_commctrl.c \
   wapi_shellapi.c \
   wapi_winbase.c \
   wapi_winuser.c \

PRG_SOURCES := \
   legacy.prg \
   oleauto.prg \
   axfunc.prg \
   wce_sim.prg \
   win_tcom.prg \
   win_os.prg \
   win_reg.prg \
   win_tprn.prg \

C_HEADERS := \
   hbwapi.h \
   hbwin.h \
   hbwinole.h \

PRG_HEADERS := \
   hbwin.ch \

ifneq ($(filter $(HB_PLATFORM),win wce),)
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
