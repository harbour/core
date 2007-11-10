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
   $(OBJ_DIR)\browodbc$(OBJEXT) \

all: \
   $(LIB_PATH) \
