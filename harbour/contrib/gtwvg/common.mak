#
# $Id$
#

LIBNAME = gtwvg

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\wvtclass$(OBJEXT) \
   $(OBJ_DIR)\wvtpaint$(OBJEXT) \
   \
   $(OBJ_DIR)\gtwvt$(OBJEXT) \
   $(OBJ_DIR)\wvtcore$(OBJEXT) \
   $(OBJ_DIR)\wvtutils$(OBJEXT) \

all: \
   $(LIB_PATH) \
