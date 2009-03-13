#
# $Id$
#

ROOT = ../../../../

LIBNAME=hbgfos2

ifeq ($(HB_ARCHITECTURE),os2)

C_SOURCES=\
	os2pm.c \

PRG_SOURCES=\
	tform.prg \
	tmenu.prg \
	tmenuit.prg \
	winctrl.prg \
	button.prg \
	edit.prg \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
