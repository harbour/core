#
# $Id$
#

LIBNAME = $(LIBPREF)hbodbc

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    sql.ch \

LIB_OBJS = \
    $(OBJ_DIR)odbc$(OBJEXT) \
    \
    $(OBJ_DIR)todbc$(OBJEXT) \
    $(OBJ_DIR)browodbc$(OBJEXT) \

all: \
    $(LIB_PATH) \
