#
# $Id$
#

LIBNAME = $(LIBPREF)gtwvg

LIB_PATH = $(LIB_DIR)/$(LIBNAME)$(LIBEXT)

C_HEADERS=\
   gtwvg.h \

PRG_HEADERS=\
   wvtwin.ch \

LIB_OBJS = \
   $(OBJ_DIR)/wvtclass$(OBJEXT) \
   $(OBJ_DIR)/wvtpaint$(OBJEXT) \
   \
   $(OBJ_DIR)/gtwvg$(OBJEXT) \
   $(OBJ_DIR)/wvtcore$(OBJEXT) \
   $(OBJ_DIR)/wvtutils$(OBJEXT) \

all: \
   $(LIB_PATH) \
