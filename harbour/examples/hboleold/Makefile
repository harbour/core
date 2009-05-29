#
# $Id$
#

ROOT = ../../

LIBNAME=hboleold

ifeq ($(HB_WITH_HBWIN),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_HBWIN=yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_HBWIN=yes
endif
endif

ifeq ($(HB_WITH_HBWIN),yes)

C_SOURCES = \
	win_ole.c \

PRG_SOURCES = \
	win_tole.prg \

else
include $(TOP)$(ROOT)config/none.cf
endif
