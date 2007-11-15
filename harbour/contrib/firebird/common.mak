#
# $Id: common.mak 7935 2007-11-10 11:31:17Z vszakats $
#

LIBNAME = firebird

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\firebird$(OBJEXT) \
   $(OBJ_DIR)\tfirebird$(OBJEXT) \

all: \
   $(LIB_PATH) \
