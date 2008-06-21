#
# $Id$
#

LIBNAME = $(LIBPREF)hbmzip

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbmzip.ch \

LIB_OBJS = \
    $(OBJ_DIR)ioapi$(OBJEXT) \
    $(OBJ_DIR)zip$(OBJEXT) \
    $(OBJ_DIR)unzip$(OBJEXT) \
    $(OBJ_DIR)hbmzip$(OBJEXT) \

all: \
    $(LIB_PATH) \
