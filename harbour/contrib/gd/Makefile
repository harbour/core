#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	gdwrp.c \

PRG_SOURCES=\
	gd.prg  \
	gdbar.prg \
	gdchart.prg \
	gdimage.prg \
	gdbarcod.prg

LIBNAME=hbgd

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_COMPILER),mingw32)
   CFLAGS := $(CFLAGS) -I/usr/include 
else
   CFLAGS := $(CFLAGS) -I/usr/include
endif
