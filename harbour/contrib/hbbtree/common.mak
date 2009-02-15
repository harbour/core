#
# $Id$
#

LIBNAME = $(LIBPREF)hbbtree

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

C_HEADERS = \
    hb_btree.h \

PRG_HEADERS = \
    hb_btree.ch \

LIB_OBJS = \
    $(OBJ_DIR)hb_btree$(OBJEXT) \
    \
    $(OBJ_DIR)tbtree$(OBJEXT) \

all: \
    $(LIB_PATH) \
