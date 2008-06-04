#
# $Id$
#

LIBNAME = $(LIBPREF)hbmsql

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    msql.ch \

LIB_OBJS = \
    $(OBJ_DIR)msql$(OBJEXT) \
    \
    $(OBJ_DIR)tmsql$(OBJEXT) \

all: \
    $(LIB_PATH) \
