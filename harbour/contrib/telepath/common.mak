#
# $Id: common.mak 7935 2007-11-10 11:31:17Z vszakats $
#

LIBNAME = telepath

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\telepath$(OBJEXT) \
   $(OBJ_DIR)\tpwin32$(OBJEXT)

all: \
   $(LIB_PATH) \
