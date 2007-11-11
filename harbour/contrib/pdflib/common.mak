#
# $Id$
#

LIBNAME = hbpdflib

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\pdfhbdocs$(OBJEXT) \
   $(OBJ_DIR)\pdfhbdoc$(OBJEXT) \
   $(OBJ_DIR)\pdf1$(OBJEXT) \

all: \
   $(LIB_PATH) \
