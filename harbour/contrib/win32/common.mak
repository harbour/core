#
# $Id$
#

LIBNAME = hbwin32

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tprinter$(OBJEXT) \
   $(OBJ_DIR)\w32_ole$(OBJEXT) \
   $(OBJ_DIR)\w32_prn$(OBJEXT) \
   \
   $(OBJ_DIR)\w32_tole$(OBJEXT) \
   $(OBJ_DIR)\w32_tprn$(OBJEXT) \

all: \
   $(LIB_PATH) \
