#
# $Id$
#

LIBNAME = $(LIBPREF)sddmy

LIB_PATH = ..\$(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)mysqldd$(OBJEXT) \

all: \
    $(LIB_PATH) \
