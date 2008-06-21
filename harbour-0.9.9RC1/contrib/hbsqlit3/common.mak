#
# $Id$
#

LIBNAME = $(LIBPREF)hbsqlit3

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbsqlit3.ch \

LIB_OBJS = \
    $(OBJ_DIR)hbsqlit3$(OBJEXT) \

all: \
    $(LIB_PATH) \
