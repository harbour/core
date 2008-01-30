#
# $Id$
#

LIBNAME = $(LIBPREF)hbtip

LIB_PATH = $(LIB_DIR)/$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    thtml.ch \
    tip.ch \

LIB_OBJS = \
    $(OBJ_DIR)/base64x$(OBJEXT) \
    $(OBJ_DIR)/encmthd$(OBJEXT) \
    $(OBJ_DIR)/utils$(OBJEXT) \
    \
    $(OBJ_DIR)/cgi$(OBJEXT) \
    $(OBJ_DIR)/client$(OBJEXT) \
    $(OBJ_DIR)/credent$(OBJEXT) \
    $(OBJ_DIR)/encb64$(OBJEXT) \
    $(OBJ_DIR)/encoder$(OBJEXT) \
    $(OBJ_DIR)/encqp$(OBJEXT) \
    $(OBJ_DIR)/encurl$(OBJEXT) \
    $(OBJ_DIR)/ftpcln$(OBJEXT) \
    $(OBJ_DIR)/httpcln$(OBJEXT) \
    $(OBJ_DIR)/mail$(OBJEXT) \
    $(OBJ_DIR)/popcln$(OBJEXT) \
    $(OBJ_DIR)/sessid$(OBJEXT) \
    $(OBJ_DIR)/smtpcln$(OBJEXT) \
    $(OBJ_DIR)/thtml$(OBJEXT) \
    $(OBJ_DIR)/url$(OBJEXT) \

all: \
    $(LIB_PATH) \
