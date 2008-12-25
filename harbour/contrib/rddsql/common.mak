#
# $Id$
#

LIBNAME = $(LIBPREF)rddsql

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)sqlbase$(OBJEXT) \
    $(OBJ_DIR)sqlmix$(OBJEXT) \

all: \
    $(LIB_PATH) \
