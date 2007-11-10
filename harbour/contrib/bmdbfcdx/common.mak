#
# $Id$
#

LIBNAME = bmdbfcdx

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\bmdbfcdx1.obj \
   $(OBJ_DIR)\bmsixcdx1.obj \

all: \
   $(LIB_PATH) \
