#
# $Id$
#

LIBNAME = msql

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    ads.ch \

LIB_OBJS = \
    $(OBJ_DIR)\msql$(OBJEXT) \
    \
    $(OBJ_DIR)\tmsql$(OBJEXT) \

all: \
    $(LIB_PATH) \
