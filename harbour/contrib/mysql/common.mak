#
# $Id$
#

LIBNAME = hbmysql

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    mysql.ch \

LIB_OBJS = \
    $(OBJ_DIR)\mysql$(OBJEXT) \
    \
    $(OBJ_DIR)\tmysql$(OBJEXT) \
    $(OBJ_DIR)\tsqlbrw$(OBJEXT) \

all: \
    $(LIB_PATH) \
