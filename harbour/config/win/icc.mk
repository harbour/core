#
# $Id$
#

# Intel(R) C/C++ Compiler
# (usage is largely compatible with msvc)

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

CC := icl.exe
CC_IN := -c
CC_OUT := -Fo

CPPFLAGS := -nologo -I. -I$(HB_INC_COMPILE) -Gs
CFLAGS :=
LDFLAGS :=

ifeq ($(HB_BUILD_MODE),c)
   CPPFLAGS += -TC
endif
ifeq ($(HB_BUILD_MODE),cpp)
   CPPFLAGS += -TP
endif

ifneq ($(HB_BUILD_WARN),no)
   CPPFLAGS += -W3
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # maximum optimizations
   # CPPFLAGS += -Ox
   CPPFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CPPFLAGS += -Zi
endif

LD := xilink.exe
LD_OUT := -out:

LIBPATHS := -libpath:$(LIB_DIR)
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += -nologo $(LIBPATHS)

AR := xilib.exe
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) -nologo -out:$(LIB_DIR)/$@ $(^F) || $(RM) $(LIB_DIR)/$@

DY := $(LD)
DFLAGS := -nologo -dll -subsystem:console $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)$(file)$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)"$(subst /,$(DIRSEP),$(DYN_DIR)/$@)" -implib:"$(IMP_FILE)" @__dyn__.tmp $(DLIBS)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
