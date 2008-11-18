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
   $(OBJ_DIR)wvgphdlr$(OBJEXT) \
   $(OBJ_DIR)wvgwnd$(OBJEXT) \
   $(OBJ_DIR)wvgcrt$(OBJEXT) \
   $(OBJ_DIR)wvgax$(OBJEXT) \
   $(OBJ_DIR)wvgdlg$(OBJEXT) \   
   \
   $(OBJ_DIR)gtwvg$(OBJEXT) \
   $(OBJ_DIR)wvgcore$(OBJEXT) \
   $(OBJ_DIR)wvgutils$(OBJEXT) \
   $(OBJ_DIR)wvgsink$(OBJEXT) \
   $(OBJ_DIR)wvgwin$(OBJEXT) \
   $(OBJ_DIR)wvggui$(OBJEXT) \

all: \
   $(LIB_PATH) \
