#
# $Id$
#

# GNU Make file for Pelles ISO C Compiler

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

CC := pocc.exe
CC_IN := -c
CC_OUT := -Fo

CFLAGS += -I. -I$(HB_HOST_INC)

CFLAGS += -Ze -Go -MT

ifeq ($(HB_COMPILER),pocc64)
   CFLAGS += -Tamd64-coff
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W1
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -Ot
   # -Ox: can cause GPF in 4.50/5.00, so it's disabled.
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
endif

RC := porc.exe
RC_OUT := -fo$(subst x,x, )

LD := polink.exe
LD_OUT := -out:

LIBPATHS := $(foreach dir,$(LIB_DIR) $(3RDLIB_DIR),-libpath:$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += -subsystem:console
LDFLAGS += $(LIBPATHS)

AR := polib.exe
AR_RULE = $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) -out:$(LIB_DIR)/$@ $(^F)

DY := $(LD)
DFLAGS += -nologo -dll $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)$(file)$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)"$(subst /,$(DIRSEP),$(DYN_DIR)/$@)" -implib:"$(IMP_FILE)" @__dyn__.tmp $(DLIBS)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
