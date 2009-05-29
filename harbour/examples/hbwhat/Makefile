#
# $Id$
#

ROOT = ../../

LIBNAME=hbwhat

ifeq ($(HB_ARCHITECTURE),win)
ifneq ($(HB_COMPILER),dmc)

C_SOURCES = \
	wincorec.c \
	whtbmp.c \
	whtbrsh.c \
	whtcdlg.c \
	whtclpb.c \
	whtcomm.c \
	whtcret.c \
	whtdate.c \
	whtdc.c \
	whtdir.c \
	whtdlg.c \
	whtdraw.c \
	whtfont.c \
	whtgdi.c \
	whthead.c \
	whticon.c \
	whtilst.c \
	whtinet.c \
	whtini.c \
	whtkbrd.c \
	whtlv.c \
	whtmain.c \
	whtmapi.c \
	whtmem.c \
	whtmenu.c \
	whtmeta.c \
	whtmisc.c \
	whtmmcap.c \
	whtmous.c \
	whtmsg.c \
	whtpen.c \
	whtprn.c \
	whtrect.c \
	whtreg.c \
	whtrgn.c \
	whtscrlb.c \
	whtseria.c \
	whtshell.c \
	whtsock.c \
	whtsys.c \
	whttab.c \
	whttbar.c \
	whttext.c \
	whttree.c \
	whtview.c \
	whtwnd.c \

PRG_SOURCES= \
	whatutil.prg \
	wincdlg.prg \
	wincomm.prg \
	wincore.prg \
	windebug.prg \
	winerror.prg \
	winini.prg \
	winrbar.prg \
	wintabs.prg \
	wintbar.prg \

PRG_HEADERS= \
	commctrl.ch \
	commdlg.ch \
	debug.ch \
	hbwhat.ch \
	import.ch \
	sqltypes.ch \
	wingdi.ch \
	wininet.ch \
	winstruc.ch \
	winuser.ch \

# This lib needs xhb headers to compile, and this is
# the hack to help Harbour find them.
HB_USER_PRGFLAGS += -I../../../../xhb

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
