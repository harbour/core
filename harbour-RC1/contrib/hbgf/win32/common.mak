#
# $Id$
#

LIBNAME = hbgfw32

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    win32.ch \

LIB_OBJS = \
    $(OBJ_DIR)win32$(OBJEXT) \
    \
    $(OBJ_DIR)button$(OBJEXT) \
    $(OBJ_DIR)edit$(OBJEXT) \
    $(OBJ_DIR)form$(OBJEXT) \
    $(OBJ_DIR)menu$(OBJEXT) \
    $(OBJ_DIR)menuitem$(OBJEXT) \
    $(OBJ_DIR)winctrl$(OBJEXT) \

all: \
    $(LIB_PATH) \
