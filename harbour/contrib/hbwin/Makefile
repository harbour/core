#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := hbwin

ifeq ($(HB_WITH_HBWIN),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_HBWIN := yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_HBWIN := yes
endif
endif

ifeq ($(HB_WITH_HBWIN),yes)

C_SOURCES := \
   olecore.c \
   oleinit.c \
   axcore.c \
   legacycd.c \
   legacyco.c \
   wce_simc.c \
   wce_smsc.c \
   win_dll.c \
   win_misc.c \
   win_osc.c \
   win_prn1.c \
   win_prn2.c \
   win_prt.c \
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
   win_os.prg \
   win_reg.prg \
   win_tprn.prg \
   win_tprt.prg \

C_HEADERS := \
   hbwapi.h \
   hbwin.h \
   hbwinole.h \

PRG_HEADERS := \
   hbwin.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
