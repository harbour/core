#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),linux)

ROOT = ../../../

C_SOURCES=\
	creawin.c \
	general.c \
	creamenu.c \
	creabutt.c \
	dispatch.c \
	msginfo.c \
	mainlgtk.c \

PRG_SOURCES=\
	form.prg \
	menu.prg \
	button.prg \
	menuitem.prg \
	winctrl.prg \

LIBNAME=hbgfgtk

C_USR += `pkg-config --cflags gtk+-2.0`


include $(TOP)$(ROOT)config/lib.cf

endif
