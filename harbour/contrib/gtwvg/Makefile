#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/hbwin

LIBNAME := gtwvg

ifeq ($(HB_WITH_GTWVG),)
   ifeq ($(HB_ARCHITECTURE),win)
      ifeq ($(HB_COMPILER),mingw)
         HB_WITH_GTWVG := yes
      endif
      ifeq ($(HB_COMPILER),msvc)
         HB_WITH_GTWVG := yes
      endif
      ifeq ($(HB_COMPILER),watcom)
         HB_WITH_GTWVG := yes
      endif
      ifeq ($(HB_COMPILER),pocc)
         HB_WITH_GTWVG := yes
      endif
      ifeq ($(HB_COMPILER),icc)
         HB_WITH_GTWVG := yes
      endif
      ifeq ($(HB_COMPILER),bcc)
         HB_WITH_GTWVG := yes
      endif
   endif
endif

ifeq ($(HB_WITH_GTWVG),yes)

C_SOURCES := \
   gtwvg.c \
   wvgcore.c \
   wvgutils.c \
   wvgwin.c \
   wvgwing.c \
   wvgsink.c \
   wvggui.c \
   wincallb.c \
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
   wincback.prg \
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

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
