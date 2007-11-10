#
# $Id$
#

LIBNAME = hbwin32

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\tprinter.obj \
   $(OBJ_DIR)\w32_ole.obj \
   $(OBJ_DIR)\w32_prn.obj \
   \
   $(OBJ_DIR)\w32_tole.obj \
   $(OBJ_DIR)\w32_tprn.obj \

all: \
   $(LIB_PATH) \
