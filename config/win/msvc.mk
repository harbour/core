OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

ifeq ($(HB_COMPILER),clang)
CC := clang-cl.exe
HB_BUILD_MODE := c
else
CC := cl.exe
endif
CC_IN := -c
CC_OUT := -Fo

CFLAGS += -I. -I$(HB_HOST_INC)

CFLAGS += -nologo

# MSVS 2005 SP1 also supports it, but we only enable it for 2008 and upper.
ifeq ($(filter $(HB_COMPILER_VER),1200 1300 1310 1400),)
   LDFLAGS += -nxcompat -dynamicbase -fixed:no
   DFLAGS += -nxcompat -dynamicbase
endif
# MSVS 2012 and upper
ifeq ($(filter $(HB_COMPILER_VER),1200 1300 1310 1400 1500 1600),)
   # some 3rd party code (libjpeg) won't compile with it
   #CFLAGS += -sdl
endif
# enable this only for users of MSVS 2013 and upper
ifeq ($(filter $(HB_COMPILER_VER),1200 1300 1310 1400 1500 1600 1700),)
   CFLAGS += -analyze
endif

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
else
   CFLAGS += -W2
endif

ifneq ($(HB_BUILD_OPTIM),no)
   ifneq ($(filter $(HB_COMPILER_VER),1200 1300 1310),)
      CFLAGS += -Ogt2yb1p -GX- -G6
   else
      CFLAGS += -O2
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
   LDFLAGS += -debug
   DFLAGS += -debug
endif

ifneq ($(filter $(HB_COMPILER_VER),1200 1300 1310),)
   ifeq ($(HB_BUILD_DEBUG),yes)
      CFLAGS += -MTd
   else
      CFLAGS += -MT
   endif
endif

RC := rc.exe
RC_OUT := -fo$(subst x,x, )
RCFLAGS += -I. -I$(HB_HOST_INC)
# Windows SDK 7.0 also supports it, but we cannot detect it.
ifeq ($(filter $(HB_COMPILER_VER),1200 1300 1310 1400 1500),)
   RCFLAGS += -nologo
endif

# # NOTE: -GA flag should be disabled when building MT _.dlls_,
# #       as it creates bad code according to MS docs [vszakats].
# ifneq ($(filter $(HB_COMPILER_VER),1200),)
#    CFLAGS += -GA
# endif

# lld.exe crashes
# ifeq ($(HB_COMPILER),clang)
# LD := lld.exe -flavor link
# else
LD := link.exe
# endif
LD_OUT := -out:

LIBPATHS := $(foreach dir,$(LIB_DIR) $(3RDLIB_DIR),-libpath:$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += -nologo -subsystem:console $(LIBPATHS)

AR := lib.exe
AR_RULE = $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) -nologo -out:$(LIB_DIR)/$@ $(^F)

DY := $(LD)
DFLAGS += -nologo -dll -subsystem:console $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

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
