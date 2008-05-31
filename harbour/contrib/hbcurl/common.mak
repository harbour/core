#
# $Id$
#

LIBNAME = $(LIBPREF)hbcurl

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbcurl.ch \

LIB_OBJS = \
    $(OBJ_DIR)hbcurl$(OBJEXT) \

all: \
    $(LIB_PATH) \
