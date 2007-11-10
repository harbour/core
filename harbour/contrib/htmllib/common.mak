#
# $Id$
#

LIBNAME = html

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\ohtm.obj \
   $(OBJ_DIR)\htmbrows.obj \
   $(OBJ_DIR)\oedit.obj \
   $(OBJ_DIR)\ofile.obj \
   $(OBJ_DIR)\jlist.obj   \
   $(OBJ_DIR)\oini.obj \
   $(OBJ_DIR)\jwindow.obj \
   $(OBJ_DIR)\ocgi.obj \
   $(OBJ_DIR)\oframe.obj \
   $(OBJ_DIR)\counter.obj \
   $(OBJ_DIR)\errorsys.obj \
   $(OBJ_DIR)\htmutil.obj \

all: \
   $(LIB_PATH) \
