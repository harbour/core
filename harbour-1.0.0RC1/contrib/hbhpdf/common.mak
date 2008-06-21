#
# $Id$
#

LIBNAME = $(LIBPREF)hbhpdf

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    harupdf.ch \

LIB_OBJS = \
    $(OBJ_DIR)harupdf$(OBJEXT) \

all: \
    $(LIB_PATH) \
