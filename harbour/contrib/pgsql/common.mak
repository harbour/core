#
# $Id$
#

LIBNAME = hbpg

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tpostgre$(OBJEXT) \
   $(OBJ_DIR)\postgres$(OBJEXT) \
   $(OBJ_DIR)\pgrdd$(OBJEXT) \

all: \
   $(LIB_PATH) \
