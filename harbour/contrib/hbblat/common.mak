#
# $Id$
#

LIBNAME = $(LIBPREF)hbblat

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    blat.ch \

LIB_OBJS = \
    $(OBJ_DIR)\blatwrp$(OBJEXT) \
    $(OBJ_DIR)\blatcls$(OBJEXT) \

all: \
    $(LIB_PATH) \
