#
# $Id$
#

LIBNAME = hbodbc

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\odbc$(OBJEXT) \
   $(OBJ_DIR)\todbc$(OBJEXT) \

all: \
   $(LIB_PATH) \
