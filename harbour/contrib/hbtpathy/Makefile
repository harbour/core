#
# $Id: Makefile,v 1.4 2005/10/13 10:45:46 mauriliolongo Exp $
#

ROOT = ../../

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_ARCHITECTURE),linux)
C_SOURCES=\
	tplinux.c \
else ($(HB_ARCHITECTURE),win32)
C_SOURCES=\
	tpwin32.c \
else
C_SOURCES=\
	tpos2.c \
endif

PRG_SOURCES=\
	telepath.prg \

PRG_HEADERS=\
	telepath.ch \

LIBNAME=hbtpathy

include $(TOP)$(ROOT)config/lib.cf

endif
