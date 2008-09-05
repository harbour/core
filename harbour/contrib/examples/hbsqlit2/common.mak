#
# $Id$
#

LIBNAME = $(LIBPREF)hbsqlit2

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbsqlit2.ch \

LIB_OBJS = \
    $(OBJ_DIR)hbsqlit2$(OBJEXT) \

all: \
    $(LIB_PATH) \
