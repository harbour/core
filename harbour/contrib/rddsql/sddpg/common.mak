#
# $Id$
#

LIBNAME = $(LIBPREF)sddpg

LIB_PATH = ..\$(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)pgsqldd$(OBJEXT) \

all: \
    $(LIB_PATH) \
