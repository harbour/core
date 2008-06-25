#
# $Id$
#

LIBNAME = $(LIBPREF)hbtpathy

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)tpwin32$(OBJEXT) \
    $(OBJ_DIR)tpcommon$(OBJEXT) \
    \
    $(OBJ_DIR)telepath$(OBJEXT) \

all: \
    $(LIB_PATH) \
