#
# $Id$
#

LIBNAME = xhb

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\hbxml$(OBJEXT)    \
   $(OBJ_DIR)\hbsyslog$(OBJEXT) \
   $(OBJ_DIR)\hboutdbg$(OBJEXT) \
   $(OBJ_DIR)\cstructc$(OBJEXT) \
   $(OBJ_DIR)\xhbfunc$(OBJEXT)  \
   $(OBJ_DIR)\txml$(OBJEXT)     \
   $(OBJ_DIR)\hblog$(OBJEXT)    \
   $(OBJ_DIR)\hblognet$(OBJEXT) \
   $(OBJ_DIR)\cstruct$(OBJEXT)  \
   $(OBJ_DIR)\xhbcomp$(OBJEXT)  \

all: \
   $(LIB_PATH) \
