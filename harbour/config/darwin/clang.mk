#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   # -ccc-clang-cxx
   HB_CMP := clang
else
   HB_CMP := clang
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
ifneq ($(filter --analyze, $(HB_USER_CFLAGS)),)
   CC_IN :=
else
   CC_IN := -c
endif
# NOTE: The ending space after -o is important, please preserve it.
#       Now solved with '$(subst x,x, )' expression.
CC_OUT := -o$(subst x,x, )

CPPFLAGS := -I. -I$(HB_INC_COMPILE) -DHB_CC_CLANG

# -fno-common enables building .dylib files
CPPFLAGS += -fno-common
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CPPFLAGS += -Wall -W
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CPPFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CPPFLAGS += -g
endif

# It's to avoid warning message generated when 'long double' is used
# remove it if you have newer compiler version
#CPPFLAGS += -Wno-long-double

LD := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := libtool
ARFLAGS :=
AR_RULE = ( $(AR) -static $(ARFLAGS) $(HB_USER_AFLAGS) -o $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && false )

DY := $(AR)
DFLAGS := -dynamic -flat_namespace -undefined warning -multiply_defined suppress -single_module $(LIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) -install_name "harbour$(DYN_EXT)" -compatibility_version $(HB_VER_MAJOR).$(HB_VER_MINOR) -current_version $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE2) && $(LN) $(@F) $(DYN_DIR)/$(DYN_PREF)$(DYNNAME2).$(HB_VER_MAJOR)$(DYN_EXT)

include $(TOP)$(ROOT)config/rules.mk
