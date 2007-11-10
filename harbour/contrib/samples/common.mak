#
# $Id$
#

LIBNAME = samples

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\environ.obj \
   $(OBJ_DIR)\date.obj   \
   $(OBJ_DIR)\dbf.obj    \
   $(OBJ_DIR)\gauge.obj  \
   $(OBJ_DIR)\num.obj    \
   $(OBJ_DIR)\stack.obj   \
   $(OBJ_DIR)\status.obj \
   $(OBJ_DIR)\time.obj

all: \
   $(LIB_PATH) \
