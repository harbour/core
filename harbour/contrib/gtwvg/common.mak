#
# $Id$
#

LIBNAME = $(LIBPREF)gtwvg

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS=\
   hbgtwvg.ch \
   wvtwin.ch \

LIB_OBJS = \
   $(OBJ_DIR)wvgclass$(OBJEXT) \
   $(OBJ_DIR)wvgpaint$(OBJEXT) \
   \
   $(OBJ_DIR)gtwvg$(OBJEXT) \
   $(OBJ_DIR)wvgcore$(OBJEXT) \
   $(OBJ_DIR)wvgutils$(OBJEXT) \

all: \
   $(LIB_PATH) \
