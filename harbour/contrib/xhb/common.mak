#
# $Id$
#

LIBNAME = $(LIBPREF)xhb

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

C_HEADERS = \
    hbcompat.h \
    hbcomprs.h \
    hbfast.h \
    hbxml.h \

PRG_HEADERS = \
    classex.ch \
    cstruct.ch \
    hbcompat.ch \
    hbctypes.ch \
    hblog.ch \
    hblogdef.ch \
    math.ch \
    usrrdd.ch \
    wintypes.ch \
    xhb.ch \
    xhbextrn.ch \

LIB_OBJS = \
    $(OBJ_DIR)hbrandom$(OBJEXT) \
    $(OBJ_DIR)freadlin$(OBJEXT) \
    $(OBJ_DIR)hbcrypt$(OBJEXT) \
    $(OBJ_DIR)hbxml$(OBJEXT) \
    $(OBJ_DIR)hbsyslog$(OBJEXT) \
    $(OBJ_DIR)hboutdbg$(OBJEXT) \
    $(OBJ_DIR)cstructc$(OBJEXT) \
    $(OBJ_DIR)xhbenum$(OBJEXT) \
    $(OBJ_DIR)xhbfunc$(OBJEXT) \
    $(OBJ_DIR)xhbmsgs$(OBJEXT) \
    $(OBJ_DIR)xhbqself$(OBJEXT) \
    $(OBJ_DIR)xhbwith$(OBJEXT) \
    $(OBJ_DIR)hbcomprs$(OBJEXT) \
    $(OBJ_DIR)hbchksum$(OBJEXT) \
    $(OBJ_DIR)xstrdel$(OBJEXT) \
    \
    $(OBJ_DIR)txml$(OBJEXT) \
    $(OBJ_DIR)hblog$(OBJEXT) \
    $(OBJ_DIR)hblognet$(OBJEXT) \
    $(OBJ_DIR)cstruct$(OBJEXT) \
    $(OBJ_DIR)xhbcomp$(OBJEXT) \

all: \
    $(LIB_PATH) \
