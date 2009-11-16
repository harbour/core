#
# $Id$
#

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

CC := cl.exe
ifeq ($(HB_COMPILER),msvcarm)
   ifneq ($(filter $(HB_COMPILER_VER),600 700 710),)
      CC := clarm.exe
   endif
endif
CC_IN := -c
CC_OUT := -Fo

CPPFLAGS := -I. -I$(HB_INC_COMPILE)
CFLAGS := -nologo -D_WIN32_WCE=0x501 -DCE_ARCH -DWINCE -D_WINCE -D_WINDOWS -D_UNICODE -D_UWIN -DUNDER_CE

ifeq ($(HB_COMPILER),msvcarm)
   CFLAGS += -DARM -D_ARM_ -DARMV4 -D_M_ARM -D_ARMV4I_ -Darmv4i -D__arm__
else ifeq ($(HB_COMPILER),msvcsh)
   CFLAGS += -D_M_SH -DSHx -D_SHX_
else ifeq ($(HB_COMPILER),msvcmips)
   CFLAGS += -D_M_MRX000=4000 -DMIPS -D_MIPS_ -DMIPS_HAS_FPU
else ifeq ($(HB_COMPILER),msvc)
   CFLAGS += -D_X86_ -D_M_IX86
endif

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
      CFLAGS += -Oxsb1 -EHsc -YX -GF
   else
      CFLAGS += -Od -Os -Gy -GS- -EHsc-
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
endif

LD := $(CC)
LD_OUT := -Fe

SYSLIBS += corelibc

LIBPATHS := -libpath:$(LIB_DIR)
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += -nologo -link -subsystem:windowsce -nodefaultlib:oldnames.lib -nodefaultlib:kernel32.lib
ifeq ($(filter $(HB_COMPILER_VER),600 700 710),)
   LDFLAGS += -manifest:no
endif
LDFLAGS += $(LIBPATHS)

AR := lib.exe
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) -nologo -out:$(LIB_DIR)/$@ $(^F) || $(RM) $(LIB_DIR)/$@

DY := $(LD)
DFLAGS := -nologo -dll -subsystem:windowsce $(LIBPATHS)
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
