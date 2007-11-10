#
# $Id$
#

LIBNAME = tip

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\base64x.obj \
   $(OBJ_DIR)\encmthd.obj \
   $(OBJ_DIR)\utils.obj \
   \
   $(OBJ_DIR)\cgi.obj \
   $(OBJ_DIR)\client.obj \
   $(OBJ_DIR)\credent.obj \
   $(OBJ_DIR)\encb64.obj \
   $(OBJ_DIR)\encoder.obj \
   $(OBJ_DIR)\encqp.obj \
   $(OBJ_DIR)\encurl.obj \
   $(OBJ_DIR)\ftpcln.obj \
   $(OBJ_DIR)\httpcln.obj \
   $(OBJ_DIR)\mail.obj \
   $(OBJ_DIR)\popcln.obj \
   $(OBJ_DIR)\smtpcln.obj \
   $(OBJ_DIR)\thtml.obj \
   $(OBJ_DIR)\url.obj \

all: \
   $(LIB_PATH) \
