#
# $Id$
#

# GNU MAKE file for xHarbour.com POCC compiler

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB
OBJ_DYN_POSTFIX := _dyn

CC := xcc.exe
CC_IN := -c
CC_OUT := -Fo

CPPFLAGS :=
CFLAGS := -I. -I$(HB_INC_COMPILE)
LDFLAGS :=

ifneq ($(HB_BUILD_OPTIM),no)
   # disabled - it produces bad code
   #CFLAGS += -Ot
endif

# For Pocket PC and ARM processors (including XScale)
#CFLAGS += /Tarm-coff

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
endif

LD := xlink.exe
LD_OUT := /out:

LIBPATHS := /libpath:$(LIB_DIR)
LDLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += $(LIBPATHS)

AR := xlib.exe
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) /out:$(LIB_DIR)/$@ $(^F)

DY := $(LD)
DFLAGS := /nologo /dll
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(SYSLIBS),$(lib)$(LIB_EXT))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)$(file)$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)"$(subst /,$(DIRSEP),$(DYN_DIR)/$@)" /implib:"$(IMP_FILE)" @__dyn__.tmp $(DLIBS)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
