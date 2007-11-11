#
# $Id$
#

LIBNAME = hgfwin32

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\button$(OBJEXT)   \
   $(OBJ_DIR)\edit$(OBJEXT)     \
   $(OBJ_DIR)\form$(OBJEXT)     \
   $(OBJ_DIR)\menu$(OBJEXT)     \
   $(OBJ_DIR)\menuitem$(OBJEXT) \
   $(OBJ_DIR)\winctrl$(OBJEXT)  \
   $(OBJ_DIR)\win32$(OBJEXT)

all: \
   $(LIB_PATH) \
