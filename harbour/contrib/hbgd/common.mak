#
# $Id$
#

LIBNAME = $(LIBPREF)hbgd

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    gd.ch \
    gdextern.ch \

LIB_OBJS = \
    $(OBJ_DIR)gdwrp$(OBJEXT) \
    $(OBJ_DIR)gd$(OBJEXT) \
    $(OBJ_DIR)gdimage$(OBJEXT) \
    $(OBJ_DIR)gdchart$(OBJEXT) \
    $(OBJ_DIR)gdbar$(OBJEXT) \
    $(OBJ_DIR)gdbarcod$(OBJEXT) \

all: \
    $(LIB_PATH) \
