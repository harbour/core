#
# $Id$
#

LIBNAME = $(LIBPREF)hbclipsm

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    time87.ch \

LIB_OBJS = \
    $(OBJ_DIR)environ$(OBJEXT) \
    $(OBJ_DIR)date$(OBJEXT) \
    $(OBJ_DIR)gauge$(OBJEXT) \
    $(OBJ_DIR)num$(OBJEXT) \
    $(OBJ_DIR)numceil$(OBJEXT) \
    $(OBJ_DIR)numfloor$(OBJEXT) \
    $(OBJ_DIR)stack$(OBJEXT) \
    $(OBJ_DIR)status$(OBJEXT) \
    $(OBJ_DIR)time$(OBJEXT) \

all: \
    $(LIB_PATH) \
