#
# $Id$
#

LIBNAME = $(LIBPREF)hbsqlit2

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    hbsqlit2.ch \

LIB_OBJS = \
    $(OBJ_DIR)hbsqlit2$(OBJEXT) \
    $(OBJ_DIR)attach$(OBJEXT) \
    $(OBJ_DIR)auth$(OBJEXT) \
    $(OBJ_DIR)btree$(OBJEXT) \
    $(OBJ_DIR)btree_rb$(OBJEXT) \
    $(OBJ_DIR)build$(OBJEXT) \
    $(OBJ_DIR)copy$(OBJEXT) \
    $(OBJ_DIR)date$(OBJEXT) \
    $(OBJ_DIR)delete$(OBJEXT) \
    $(OBJ_DIR)encode$(OBJEXT) \
    $(OBJ_DIR)expr$(OBJEXT) \
    $(OBJ_DIR)func$(OBJEXT) \
    $(OBJ_DIR)hash$(OBJEXT) \
    $(OBJ_DIR)insert$(OBJEXT) \
    $(OBJ_DIR)main$(OBJEXT) \
    $(OBJ_DIR)opcodes$(OBJEXT) \
    $(OBJ_DIR)os$(OBJEXT) \
    $(OBJ_DIR)pager$(OBJEXT) \
    $(OBJ_DIR)parse$(OBJEXT) \
    $(OBJ_DIR)pragma$(OBJEXT) \
    $(OBJ_DIR)printf$(OBJEXT) \
    $(OBJ_DIR)random$(OBJEXT) \
    $(OBJ_DIR)select$(OBJEXT) \
    $(OBJ_DIR)table$(OBJEXT) \
    $(OBJ_DIR)tokenize$(OBJEXT) \
    $(OBJ_DIR)trigger$(OBJEXT) \
    $(OBJ_DIR)update$(OBJEXT) \
    $(OBJ_DIR)util$(OBJEXT) \
    $(OBJ_DIR)vacuum$(OBJEXT) \
    $(OBJ_DIR)vdbe$(OBJEXT) \
    $(OBJ_DIR)vdbeaux$(OBJEXT) \
    $(OBJ_DIR)where$(OBJEXT) \

all: \
    $(LIB_PATH) \
