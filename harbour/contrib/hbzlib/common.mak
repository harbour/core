#
# $Id$
#

LIBNAME = $(LIBPREF)hbzlib

LIB_PATH = $(LIB_DIR)/$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbzlib.ch \

LIB_OBJS = \
    $(OBJ_DIR)/ioapi$(OBJEXT) \
    $(OBJ_DIR)/zip$(OBJEXT) \
    $(OBJ_DIR)/unzip$(OBJEXT) \
    $(OBJ_DIR)/hbzlib$(OBJEXT) \
    $(OBJ_DIR)/hbmzip$(OBJEXT) \

all: \
    $(LIB_PATH) \
