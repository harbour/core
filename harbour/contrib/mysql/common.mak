#
# $Id$
#

LIBNAME = mysql

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tmysql.obj \
   $(OBJ_DIR)\tsqlbrw.obj \
   $(OBJ_DIR)\mysql.obj

all: \
   $(LIB_PATH) \
