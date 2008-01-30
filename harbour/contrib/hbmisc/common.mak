#
# $Id$
#

LIBNAME = $(LIBPREF)hbmisc

LIB_PATH = $(LIB_DIR)/$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)/dates2$(OBJEXT) \
    $(OBJ_DIR)/dbftools$(OBJEXT) \
    $(OBJ_DIR)/hb_f$(OBJEXT) \
    $(OBJ_DIR)/mathx$(OBJEXT) \
    $(OBJ_DIR)/strfmt$(OBJEXT) \
    $(OBJ_DIR)/stringsx$(OBJEXT) \
    \
    $(OBJ_DIR)/fileread$(OBJEXT) \
    $(OBJ_DIR)/nconvert$(OBJEXT) \
    $(OBJ_DIR)/numtxten$(OBJEXT) \
    $(OBJ_DIR)/numtxthu$(OBJEXT) \
    $(OBJ_DIR)/stringp$(OBJEXT) \
    $(OBJ_DIR)/twirler$(OBJEXT) \

all: \
    $(LIB_PATH) \
