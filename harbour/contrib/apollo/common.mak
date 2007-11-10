#
# $Id$
#

LIBNAME = apollo

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\apollo.obj \
   $(OBJ_DIR)\apollo1.obj \

all: \
   $(LIB_PATH) \
