#
# $Id$
#

LIBNAME = $(LIBPREF)hbw32

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

C_HEADERS = \
    hbwin.h \

PRG_HEADERS = \
    hbw32.ch \

LIB_OBJS = \
    $(OBJ_DIR)win_dll$(OBJEXT) \
    $(OBJ_DIR)win_ole$(OBJEXT) \
    $(OBJ_DIR)win_osc$(OBJEXT) \
    $(OBJ_DIR)win_prn1$(OBJEXT) \
    $(OBJ_DIR)win_prn2$(OBJEXT) \
    $(OBJ_DIR)win_prt$(OBJEXT) \
    $(OBJ_DIR)win_regc$(OBJEXT) \
    \
    $(OBJ_DIR)win_os$(OBJEXT) \
    $(OBJ_DIR)win_reg$(OBJEXT) \
    $(OBJ_DIR)win_tole$(OBJEXT) \
    $(OBJ_DIR)win_tprn$(OBJEXT) \
    $(OBJ_DIR)win_tprt$(OBJEXT) \

all: \
    $(LIB_PATH) \
