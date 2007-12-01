#
# $Id$
#

LIBNAME = hbpgsql

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    postgres.ch \

LIB_OBJS = \
    $(OBJ_DIR)\postgres$(OBJEXT) \
    \
    $(OBJ_DIR)\tpostgre$(OBJEXT) \
    $(OBJ_DIR)\pgrdd$(OBJEXT) \

all: \
    $(LIB_PATH) \
