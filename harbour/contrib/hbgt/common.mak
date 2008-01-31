#
# $Id$
#

LIBNAME = $(LIBPREF)hbgt

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)asciisum$(OBJEXT) \
    $(OBJ_DIR)ascpos$(OBJEXT) \
    $(OBJ_DIR)atdiff$(OBJEXT) \
    $(OBJ_DIR)chareven$(OBJEXT) \
    $(OBJ_DIR)charmix$(OBJEXT) \
    $(OBJ_DIR)charodd$(OBJEXT) \
    $(OBJ_DIR)chrcount$(OBJEXT) \
    $(OBJ_DIR)chrfirst$(OBJEXT) \
    $(OBJ_DIR)chrtotal$(OBJEXT) \
    $(OBJ_DIR)strasint$(OBJEXT) \
    $(OBJ_DIR)strcount$(OBJEXT) \
    $(OBJ_DIR)strcspn$(OBJEXT) \
    $(OBJ_DIR)strdiff$(OBJEXT) \
    $(OBJ_DIR)strexpan$(OBJEXT) \
    $(OBJ_DIR)strleft$(OBJEXT) \
    $(OBJ_DIR)strpbrk$(OBJEXT) \
    $(OBJ_DIR)strright$(OBJEXT) \

all: \
    $(LIB_PATH) \
