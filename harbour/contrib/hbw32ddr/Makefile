#
# $Id$
#

# Needs Windows official ddraw.h to be copied to this directory, 
# in order to compile under MingW. [vszakats]

ROOT = ../../

LIBNAME=hbw32ddr

ifeq ($(HB_ARCHITECTURE),w32)

CPP_SOURCES=\
	w32_ddrw.cpp \

PRG_SOURCES=\

include $(TOP)$(ROOT)config/lib.cf

C_USR += -I.

else
include $(TOP)$(ROOT)config/none.cf
endif
