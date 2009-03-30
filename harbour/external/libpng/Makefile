#
# $Id$
#

HB_BUILD_WARN = no
HB_USER_CFLAGS += -I$(TOP)$(ROOT)source/hbzlib

ROOT = ../../

LIBNAME=libpng

ifeq ($(HB_WITH_LIBPNG),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_LIBPNG=yes
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITH_LIBPNG=yes
endif
ifeq ($(HB_ARCHITECTURE),dos)
HB_WITH_LIBPNG=yes
endif
ifeq ($(HB_ARCHITECTURE),poccce)
HB_WITH_LIBPNG=no
endif
endif

ifeq ($(HB_WITH_LIBPNG),yes)

C_SOURCES =\
	png.c \
	pngerror.c \
	pnggccrd.c \
	pngget.c \
	pngmem.c \
	pngpread.c \
	pngread.c \
	pngrio.c \
	pngrtran.c \
	pngrutil.c \
	pngset.c \
	pngtrans.c \
	pngvcrd.c \
	pngwio.c \
	pngwrite.c \
	pngwtran.c \
	pngwutil.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
