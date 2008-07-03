#
# $Id$
#

LIBNAME = $(LIBPREF)hbbmcdx

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)bmdbfcdx$(OBJEXT) \
    $(OBJ_DIR)bmsixcdx$(OBJEXT) \

all: \
    $(LIB_PATH) \
