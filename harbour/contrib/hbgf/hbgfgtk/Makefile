#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),linux)

ifeq ($(HB_INC_GTK),)
HB_INC_GTK = /usr/include
endif

HB_INC_GTK_OK += $(foreach d, $(HB_INC_GTK), $(if $(wildcard $(d)/glib.h),$(d),))

ifneq ($(strip $(HB_INC_GTK_OK)),)

C_USR += $(foreach d, $(HB_INC_GTK_OK), -I$(d))
C_USR += `pkg-config --cflags gtk+-2.0`

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

include $(TOP)$(ROOT)config/lib.cf

endif
endif
