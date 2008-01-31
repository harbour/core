#
# $Id: common.mak 7935 2007-11-10 11:31:17Z vszakats $
#

LIBNAME = $(LIBPREF)hbtpathy

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)tpwin32$(OBJEXT) \
    \
    $(OBJ_DIR)telepath$(OBJEXT) \

all: \
    $(LIB_PATH) \
