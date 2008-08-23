#
# $Id$
#

LIBNAME = $(LIBPREF)gtalleg

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)gtalleg$(OBJEXT) \
    $(OBJ_DIR)ssf$(OBJEXT) \

all: \
    $(LIB_PATH) \
