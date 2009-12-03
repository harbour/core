#
# $Id$
#

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

ifeq ($(HB_CPU),x86)
   # Always compile in -UNICODE mode for MSVC 9.0 and upper
   # These versions don't support Win9x anymore, so it's safe. [vszakats]
   ifeq ($(filter $(HB_COMPILER_VER),600 700 710 800),)
      HB_CFLAGS += -DUNICODE
   endif
endif

HB_DYN_COPT := -DHB_DYNLIB

CC := cl.exe
CC_IN := -c
CC_OUT := -Fo

CFLAGS := -nologo -I. -I$(HB_INC_COMPILE) -Gs
LDFLAGS :=

ifeq ($(HB_BUILD_MODE),c)
   CFLAGS += -TC
endif
ifeq ($(HB_BUILD_MODE),cpp)
   CFLAGS += -TP
endif
# Build in C++ mode by default
ifeq ($(HB_BUILD_MODE),)
   CFLAGS += -TP
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W4 -wd4127
endif

ifneq ($(HB_BUILD_OPTIM),no)
   ifneq ($(filter $(HB_COMPILER_VER),600 700 710),)
      CFLAGS += -Ogt2yb1p -GX- -G6 -YX
   else
      CFLAGS += -Ot2b1 -EHs-c-
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
   LDFLAGS += -debug
endif

ifneq ($(filter $(HB_COMPILER_VER),600 700 710),)
   ifeq ($(HB_BUILD_DEBUG),yes)
      CFLAGS += -MTd
   else
      CFLAGS += -MT
   endif
endif

# # NOTE: -GA flag should be disabled when building MT _.dlls_,
# #       as it creates bad code according to MS docs [vszakats].
# ifneq ($(filter $(HB_COMPILER_VER),600),)
#    CFLAGS += -GA
# endif

LD := link.exe
LD_OUT := -out:

LIBPATHS := -libpath:$(LIB_DIR)
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += -nologo $(LIBPATHS)

AR := lib.exe
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
