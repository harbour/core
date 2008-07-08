#
# $Id$
#

LIBNAME = $(LIBPREF)hbw32

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbw32.ch \

LIB_OBJS = \
    $(OBJ_DIR)dllcall$(OBJEXT) \
    $(OBJ_DIR)tprinter$(OBJEXT) \
    $(OBJ_DIR)w32_ole$(OBJEXT) \
    $(OBJ_DIR)w32_osc$(OBJEXT) \
    $(OBJ_DIR)w32_prn$(OBJEXT) \
    $(OBJ_DIR)w32_regc$(OBJEXT) \
    \
    $(OBJ_DIR)w32_os$(OBJEXT) \
    $(OBJ_DIR)w32_reg$(OBJEXT) \
    $(OBJ_DIR)w32_tole$(OBJEXT) \
    $(OBJ_DIR)w32_tprn$(OBJEXT) \

all: \
    $(LIB_PATH) \
