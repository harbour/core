#
# $Id$
#

HB_USER_PRGFLAGS += -I$(TOP)$(ROOT)contrib/hbqt

ROOT = ../../

LIBNAME=hbxbp

ifeq ($(HB_WITH_QT),)
HB_WITH_QT=yes
ifeq ($(HB_ARCHITECTURE),dos)
HB_WITH_QT=no
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),mingw64)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),watcom)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),bcc)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),pocc)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),pocc64)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),poccarm)
HB_WITH_QT=no
endif
endif

ifeq ($(HB_WITH_QT),yes)

PRG_SOURCES=\
	xbp_temp.prg \
	xbpgeneric.prg \
	xbpdialog.prg \
	xbpwindow.prg \
	xbpparthandler.prg \
	xbpmenubar.prg \
	xbptoolbar.prg \
	xbppushbutton.prg \
	xbpdataref.prg \
	xbpcheckbox.prg \
	xbp3state.prg \
	xbpradiobutton.prg \
	xbptabpage.prg \
	xbplistbox.prg \
	xbpstatusbar.prg \
	xbpscrollbar.prg \

PRG_HEADERS=\
	xbp.ch \
	appevent.ch \
	apig.ch \
	gra.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
