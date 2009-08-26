#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   # for old Darwin systems (having GCC 2.95) this may need to be
   # redefined to 'cc'.
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
# NOTE: The ending space after -o is important, please preserve it.
#       Now solved with '$(subst x,x, )' expression.
CC_OUT := -o$(subst x,x, )

CPPFLAGS := -I. -I$(HB_INC_COMPILE)

# -no-cpp-precomp prevents from using buggy precompiled headers
CPPFLAGS += -no-cpp-precomp

# -fno-common enables building .dylib files
CFLAGS := -fno-common
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -Wall -W
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

# It's to avoid warning message generated when 'long double' is used
# remove it if you have newer compiler version
#CFLAGS += -Wno-long-double

LD := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(LIB_DIR)

LDLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),-l$(lib))
LDFLAGS += $(foreach dir,$(LIBPATHS) $(SYSLIBPATHS),-L$(dir))

AR := libtool
ARFLAGS :=
AR_RULE = $(AR) -static $(ARFLAGS) $(HB_USER_AFLAGS) -o $(LIB_DIR)/$@ $(^F) || ( $(RM) $(LIB_DIR)/$@ && false )

DY := $(AR)
DFLAGS := -dynamic -flat_namespace -undefined warning -multiply_defined suppress -single_module
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) -install_name "harbour$(DYN_EXT)" -compatibility_version $(HB_VER_MAJOR).$(HB_VER_MINOR) -current_version $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ -filelist __dyn__.tmp $(DLIBS)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
