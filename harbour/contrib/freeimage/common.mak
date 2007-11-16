#
# $Id$
#

LIBNAME = fi_lib

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\fi_winfu$(OBJEXT) \
   $(OBJ_DIR)\fi_wrp$(OBJEXT) \

all: \
   $(LIB_PATH) \
