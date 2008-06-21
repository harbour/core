#
# $Id$
#

LIBNAME = $(LIBPREF)hbw32ddr

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)w32_ddrw$(OBJEXT) \

all: \
    $(LIB_PATH) \
