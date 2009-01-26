#
# $Id$
#

LIBNAME = $(LIBPREF)hbssl

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbssl.ch \

LIB_OBJS = \
    $(OBJ_DIR)ssl$(OBJEXT) \
    $(OBJ_DIR)sslctx$(OBJEXT) \
    $(OBJ_DIR)sslrand$(OBJEXT) \

all: \
    $(LIB_PATH) \
