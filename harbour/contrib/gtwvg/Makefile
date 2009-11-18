#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtwvg

C_SOURCES := \
   gtwvg.c \
   wvgcore.c \
   wvgutils.c \
   wvgwin.c \
   wvgwing.c \
   wvgsink.c \
   wvggui.c \
   wvgcuig.c \

PRG_SOURCES := \
   wvgclass.prg \
   wvgpaint.prg \
   wvgphdlr.prg \
   wvgwnd.prg \
   wvgcrt.prg \
   wvgax.prg \
   wvgdlg.prg \
   wvgmenub.prg \
   wvgtoolb.prg \
   wvgcombo.prg \
   wvglistb.prg \
   wvgpushb.prg \
   wvgstatb.prg \
   wvgstatc.prg \
   wvgtreev.prg \
   wvgcheck.prg \
   wvgdatar.prg \
   wvgradio.prg \
   wvg3stat.prg \
   wvgbitmp.prg \
   wvgsle.prg \
   wvgmle.prg \
   wvghtmlv.prg \
   wvgsysw.prg \
   wvgdarea.prg \
   wvgscrlb.prg \
   wvgtabpg.prg \

PRG_HEADERS := \
   hbgtwvg.ch \
   wvtwin.ch \
   wvgparts.ch \

ifeq ($(HB_PLATFORM),win)
   ifeq ($(filter $(HB_COMPILER),xcc),)
   
      HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/hbwin
   
      include $(TOP)$(ROOT)config/header.mk
      include $(TOP)$(ROOT)config/lib.mk
   else
      HB_SKIP_REASON := compiler not supported
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
