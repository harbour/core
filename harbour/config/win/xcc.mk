#
# $Id$
#

# GNU MAKE file for xHarbour.com POCC compiler

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

CC := xcc.exe
CC_IN := -c
CC_OUT := -Fo

CFLAGS += -I. -I$(HB_INC_COMPILE) -I$(TOP). -I$(TOP)..
ifeq ($(HB_SHELL),sh)
   CFLAGS := $(subst /,\\,$(CFLAGS))
else
   CFLAGS := $(subst /,\,$(CFLAGS))
endif

CFLAGS += -MT

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W1
else
   CFLAGS += -W0
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # disabled - it produces bad code
   #CFLAGS += -Ot
endif

# For Pocket PC and ARM processors (including XScale)
#CFLAGS += -Tarm-coff

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
endif

LD := xlink.exe
LD_OUT := -out:

LIBPATHS := -libpath:$(LIB_DIR)
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += $(LIBPATHS)

AR := xlib.exe
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) -out:$(LIB_DIR)/$@ $(^F)

DY := $(LD)
DFLAGS += -nologo -dll $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

ifeq ($(HB_SHELL),sh)
   DYNFIX = && mv $(DYN_DIR)/$(@:.dll=.LIB) $(LIB_DIR)/$(@:.dll=.lib) && $(RM) $(DYN_DIR)/$(@:.dll=.EXP)
else
   DYNFIX :=
endif

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)$(subst /,\,$(file))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)"$(subst /,\,$(DYN_DIR)/$@)" @__dyn__.tmp $(DLIBS) $(DYNFIX)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
