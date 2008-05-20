#
# $Id$
#

LIBNAME = $(LIBPREF)hbvpdf

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbvpdf.ch \

LIB_OBJS = \
    $(OBJ_DIR)hbvpdf$(OBJEXT) \
    $(OBJ_DIR)hbvpdft$(OBJEXT) \

all: \
    $(LIB_PATH) \
