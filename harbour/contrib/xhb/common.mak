#
# $Id$
#

LIBNAME = $(LIBPREF)xhb

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

C_HEADERS = \
    hbcompat.h \
    xhb.h \

PRG_HEADERS = \
    hbcompat.ch \
    xhb.ch \

LIB_OBJS = \
    $(OBJ_DIR)dbf2txt$(OBJEXT) \
    $(OBJ_DIR)filestat$(OBJEXT) \
    $(OBJ_DIR)freadlin$(OBJEXT) \
    $(OBJ_DIR)hbcrypt$(OBJEXT) \
    $(OBJ_DIR)hbxml$(OBJEXT) \
    $(OBJ_DIR)hbsyslog$(OBJEXT) \
    $(OBJ_DIR)hboutdbg$(OBJEXT) \
    $(OBJ_DIR)cstructc$(OBJEXT) \
    $(OBJ_DIR)xhbarr$(OBJEXT) \
    $(OBJ_DIR)xhbcopyf$(OBJEXT) \
    $(OBJ_DIR)xhbenum$(OBJEXT) \
    $(OBJ_DIR)xhbfunc$(OBJEXT) \
    $(OBJ_DIR)xhbmsgs$(OBJEXT) \
    $(OBJ_DIR)xhbqself$(OBJEXT) \
    $(OBJ_DIR)xhbwith$(OBJEXT) \
    $(OBJ_DIR)hbcomprs$(OBJEXT) \
    $(OBJ_DIR)xstrdel$(OBJEXT) \
    $(OBJ_DIR)dbgfxc$(OBJEXT) \
    \
    $(OBJ_DIR)arrayblk$(OBJEXT) \
    $(OBJ_DIR)txml$(OBJEXT) \
    $(OBJ_DIR)hblog$(OBJEXT) \
    $(OBJ_DIR)hblognet$(OBJEXT) \
    $(OBJ_DIR)cstruct$(OBJEXT) \
    $(OBJ_DIR)hbstruct$(OBJEXT) \
    $(OBJ_DIR)dirrec$(OBJEXT) \
    $(OBJ_DIR)xhbcomp$(OBJEXT) \
    $(OBJ_DIR)dbgfx$(OBJEXT) \
    $(OBJ_DIR)sprintf$(OBJEXT) \
    $(OBJ_DIR)regexrpl$(OBJEXT) \

all: \
    $(LIB_PATH) \
