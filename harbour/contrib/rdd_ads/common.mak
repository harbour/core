#
# $Id$
#

LIBNAME = rddads

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\ads1.obj    \
   $(OBJ_DIR)\adsfunc.obj \
   $(OBJ_DIR)\adsmgmnt.obj

all: \
   $(LIB_PATH) \
