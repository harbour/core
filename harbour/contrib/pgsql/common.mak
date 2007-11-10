#
# $Id$
#

LIBNAME = libhbpg

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tpostgre.obj \
   $(OBJ_DIR)\postgres.obj

all: \
   $(LIB_PATH) \
