#
# $Id$
#

LIBNAME = $(LIBPREF)sddfb

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)fbirddd$(OBJEXT) \

all: \
    $(LIB_PATH) \
