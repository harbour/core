#
# $Id$
#

LIBNAME = mysql

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tmysql$(OBJEXT) \
   $(OBJ_DIR)\tsqlbrw$(OBJEXT) \
   $(OBJ_DIR)\mysql$(OBJEXT)

all: \
   $(LIB_PATH) \
