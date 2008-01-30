#
# $Id$
#

LIBNAME = $(LIBPREF)rddado

LIB_PATH = $(LIB_DIR)/$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    adordd.ch \

LIB_OBJS = \
    $(OBJ_DIR)/adordd$(OBJEXT)

all: \
    $(LIB_PATH) \
