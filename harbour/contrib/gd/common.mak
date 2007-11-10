#
# $Id$
#

LIBNAME = hbgd

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\gdwrp.obj \
   $(OBJ_DIR)\gd.obj \
   $(OBJ_DIR)\gdimage.obj \
   $(OBJ_DIR)\gdchart.obj \
   $(OBJ_DIR)\gdbar.obj \
   $(OBJ_DIR)\gdbarcod.obj \

all: \
   $(LIB_PATH) \
