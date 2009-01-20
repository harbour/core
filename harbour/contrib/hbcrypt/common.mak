#
# $Id$
#

LIBNAME = $(LIBPREF)hbcrypt

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)hbsha2$(OBJEXT) \
    $(OBJ_DIR)hbsha2hm$(OBJEXT) \
    $(OBJ_DIR)sha1$(OBJEXT) \
    $(OBJ_DIR)sha1hmac$(OBJEXT) \
    $(OBJ_DIR)sha2$(OBJEXT) \
    $(OBJ_DIR)sha2hmac$(OBJEXT) \

all: \
    $(LIB_PATH) \
