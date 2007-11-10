#
# $Id$
#

LIBNAME = hbwin32ddrw

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\w32_ddrw$(OBJEXT) \

all: \
   $(LIB_PATH) \
