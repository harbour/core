#
# $Id$
#

LIBNAME = $(LIBPREF)hbfimage

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    freeimage.ch \

LIB_OBJS = \
    $(OBJ_DIR)fi_winfu$(OBJEXT) \
    $(OBJ_DIR)fi_wrp$(OBJEXT) \

all: \
    $(LIB_PATH) \
