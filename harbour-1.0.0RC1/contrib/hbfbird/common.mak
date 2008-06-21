#
# $Id$
#

LIBNAME = $(LIBPREF)hbfbird

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)firebird$(OBJEXT) \
    $(OBJ_DIR)tfirebird$(OBJEXT) \

all: \
    $(LIB_PATH) \
