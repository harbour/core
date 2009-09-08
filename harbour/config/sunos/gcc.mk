#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
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
CFLAGS :=
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -Wall -W
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O3
endif

CFLAGS += -fPIC

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o

LIBPATHS := $(LIB_DIR)

LDLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),-l$(lib))
LDFLAGS += $(foreach dir,$(LIBPATHS) $(SYSLIBPATHS),-L$(dir))

AR := $(HB_CCPREFIX)ar
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) rc $(LIB_DIR)/$@ $(^F) || ( $(RM) $(LIB_DIR)/$@ && false )

DY := $(CC)
DFLAGS := -shared -fPIC $(foreach dir,$(SYSLIBPATHS),-L$(dir))
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS)

include $(TOP)$(ROOT)config/rules.mk
