#
# $Id$
#

LIBNAME = html

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)\ohtm$(OBJEXT) \
    $(OBJ_DIR)\htmbrows$(OBJEXT) \
    $(OBJ_DIR)\oedit$(OBJEXT) \
    $(OBJ_DIR)\ofile$(OBJEXT) \
    $(OBJ_DIR)\jlist$(OBJEXT) \
    $(OBJ_DIR)\oini$(OBJEXT) \
    $(OBJ_DIR)\jwindow$(OBJEXT) \
    $(OBJ_DIR)\ocgi$(OBJEXT) \
    $(OBJ_DIR)\oframe$(OBJEXT) \
    $(OBJ_DIR)\counter$(OBJEXT) \
    $(OBJ_DIR)\errorsys$(OBJEXT) \
    $(OBJ_DIR)\htmutil$(OBJEXT) \

all: \
    $(LIB_PATH) \
