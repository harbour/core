#
# $Id: common.mak 7935 2007-11-10 11:31:17Z vszakats $
#

LIBNAME = hbdbgfx

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\dbgfxc$(OBJEXT) \
   $(OBJ_DIR)\dbgfx$(OBJEXT) \
   $(OBJ_DIR)\sprintf$(OBJEXT) \

all: \
   $(LIB_PATH) \
