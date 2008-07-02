#
# $Id$
#

ROOT = ../../../

LIBNAME=hbgfw32

ifeq ($(HB_ARCHITECTURE),w32)

C_SOURCES=\
	win32.c \

PRG_SOURCES=\
	button.prg \
	edit.prg \
	form.prg \
	menu.prg \
	menuitem.prg \
	winctrl.prg \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
