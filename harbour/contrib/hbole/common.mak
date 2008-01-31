#
# $Id$
#

LIBNAME = $(LIBPREF)hbole

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)ole2$(OBJEXT) \
    \
    $(OBJ_DIR)oleauto$(OBJEXT) \

all: \
    $(LIB_PATH) \
