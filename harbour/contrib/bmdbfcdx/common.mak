#
# $Id$
#

LIBNAME = bmdbfcdx

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)\bmdbfcdx1$(OBJEXT) \
    $(OBJ_DIR)\bmsixcdx1$(OBJEXT) \

all: \
    $(LIB_PATH) \
