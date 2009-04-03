#
# $Id$
#

ROOT = ../../

LIBNAME=hbole

ifeq ($(HB_WITH_HBOLE),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_HBOLE=yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_HBOLE=yes
endif
endif

ifeq ($(HB_WITH_HBOLE),yes)

C_SOURCES=\
	oleinit.c \
	olecore.c \

PRG_SOURCES=\
	oleauto.prg \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
