#
# $Id$
#

LIBNAME = $(LIBPREF)rddads

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    ads.ch \

LIB_OBJS = \
    $(OBJ_DIR)ads1$(OBJEXT) \
    $(OBJ_DIR)adsfunc$(OBJEXT) \
    $(OBJ_DIR)adsmgmnt$(OBJEXT) \

all: \
    $(LIB_PATH) \
