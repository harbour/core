#
# $Id$
#

LIBNAME = hb_btree

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\hb_btree.obj \
   $(OBJ_DIR)\tbtree.obj \

all: \
   $(LIB_PATH) \
