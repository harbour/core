#
# $Id$
#

ROOT = ../../../

LIBNAME=hbgfgtk

ifeq ($(HB_ARCHITECTURE),linux)

ifeq ($(HB_INC_GTK),)
ifeq ($(HB_XBUILD),)
HB_INC_GTK = /usr/include /usr/include/glib-2.0/
endif
endif
HB_INC_GTK_OK += $(foreach d, $(HB_INC_GTK), $(if $(wildcard $(d)/glib.h),$(d),))

endif

ifneq ($(strip $(HB_INC_GTK_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_GTK_OK), -I$(d))
HB_USER_CFLAGS += `pkg-config --cflags gtk+-2.0`

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

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
