#
# $Id$
#

LIBNAME = libmisc

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\dates2.obj \
   $(OBJ_DIR)\dbftools.obj \
   $(OBJ_DIR)\hb_f.obj \
   $(OBJ_DIR)\mathx.obj \
   $(OBJ_DIR)\strfmt.obj \
   $(OBJ_DIR)\stringsx.obj \
   \
   $(OBJ_DIR)\fileread.obj \
   $(OBJ_DIR)\nconvert.obj \
   $(OBJ_DIR)\numtxten.obj \
   $(OBJ_DIR)\numtxthu.obj \
   $(OBJ_DIR)\stringp.obj \
   $(OBJ_DIR)\twirler.obj

all: \
   $(LIB_PATH) \
