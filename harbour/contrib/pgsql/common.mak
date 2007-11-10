#
# $Id$
#

LIBNAME = libhbpg

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tpostgre$(OBJEXT) \
   $(OBJ_DIR)\postgres$(OBJEXT)

all: \
   $(LIB_PATH) \
