ifeq ($(HB_BUILD_MODE),cpp)
   ifneq ($(findstring clang$(subst x, ,x)version$(subst x, ,x)1,$(shell clang --version)),)
      HB_BUILD_MODE := c
   endif
endif

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := clang++
else
   HB_CMP := clang
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

ifeq ($(filter $(HB_PLATFORM),darwin win),)
   HB_DYN_COPT := -DHB_DYNLIB -fPIC
endif

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
ifneq ($(filter --analyze, $(HB_USER_CFLAGS)),)
   CC_IN :=
else
   CC_IN := -c
endif
# NOTE: Works also without the ending space after -o.
CC_OUT := -o$(subst x,x, )

CFLAGS += -I. -I$(HB_HOST_INC)

# -fno-common enables building .dylib files
CFLAGS += -fno-common

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
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

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))

LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))
LDFLAGS += $(LIBPATHS)

DY := $(CC)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))
DFLAGS += $(LIBPATHS)

include $(TOP)$(ROOT)config/rules.mk
