#
# $Id$
#

LIBNAME = gt

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\asciisum.obj \
   $(OBJ_DIR)\ascpos.obj \
   $(OBJ_DIR)\atdiff.obj \
   $(OBJ_DIR)\chareven.obj \
   $(OBJ_DIR)\charmix.obj \
   $(OBJ_DIR)\charodd.obj \
   $(OBJ_DIR)\chrcount.obj \
   $(OBJ_DIR)\chrfirst.obj \
   $(OBJ_DIR)\chrtotal.obj \
   $(OBJ_DIR)\strasint.obj \
   $(OBJ_DIR)\strcount.obj \
   $(OBJ_DIR)\strcspn.obj \
   $(OBJ_DIR)\strdiff.obj \
   $(OBJ_DIR)\strexpan.obj \
   $(OBJ_DIR)\strleft.obj \
   $(OBJ_DIR)\strpbrk.obj \
   $(OBJ_DIR)\strright.obj

all: \
   $(LIB_PATH) \
