#
# $Id$
#

LIBNAME = $(LIBPREF)gtwvg

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS=\
   hbgtwvg.ch \
   wvtwin.ch \
   wvgparts.ch \

LIB_OBJS = \
   $(OBJ_DIR)wvgclass$(OBJEXT) \
   $(OBJ_DIR)wvgpaint$(OBJEXT) \
   $(OBJ_DIR)wvgphdlr$(OBJEXT) \
   $(OBJ_DIR)wvgwnd$(OBJEXT) \
   $(OBJ_DIR)wvgcrt$(OBJEXT) \
   $(OBJ_DIR)wvgax$(OBJEXT) \
   $(OBJ_DIR)wvgdlg$(OBJEXT) \
   $(OBJ_DIR)wvgmenub$(OBJEXT) \
   $(OBJ_DIR)wvgtoolb$(OBJEXT) \
   $(OBJ_DIR)wvgcombo$(OBJEXT) \
   $(OBJ_DIR)wvglistb$(OBJEXT) \
   $(OBJ_DIR)wvgpushb$(OBJEXT) \
   $(OBJ_DIR)wvgstatb$(OBJEXT) \
   $(OBJ_DIR)wvgstatc$(OBJEXT) \
   $(OBJ_DIR)wvgtreev$(OBJEXT) \
   $(OBJ_DIR)wincback$(OBJEXT) \
   $(OBJ_DIR)wvgcheck$(OBJEXT) \
   $(OBJ_DIR)wvgdatar$(OBJEXT) \
   $(OBJ_DIR)wvgradio$(OBJEXT) \
   $(OBJ_DIR)wvg3stat$(OBJEXT) \
   $(OBJ_DIR)wvgbitmp$(OBJEXT) \
   $(OBJ_DIR)wvgsle$(OBJEXT) \
   $(OBJ_DIR)wvgmle$(OBJEXT) \
   $(OBJ_DIR)wvghtmlv$(OBJEXT) \
   $(OBJ_DIR)wvgsysw$(OBJEXT) \
   $(OBJ_DIR)wvgdarea$(OBJEXT) \
   $(OBJ_DIR)wvgscrlb$(OBJEXT) \
   \
   $(OBJ_DIR)gtwvg$(OBJEXT) \
   $(OBJ_DIR)wvgcore$(OBJEXT) \
   $(OBJ_DIR)wvgutils$(OBJEXT) \
   $(OBJ_DIR)wvgsink$(OBJEXT) \
   $(OBJ_DIR)wvgwin$(OBJEXT) \
   $(OBJ_DIR)wvggui$(OBJEXT) \
   $(OBJ_DIR)wincallb$(OBJEXT) \
   $(OBJ_DIR)wvgcuig$(OBJEXT) \


all: \
   $(LIB_PATH) \
