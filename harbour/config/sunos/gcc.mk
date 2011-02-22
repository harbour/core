#
# $Id$
#

ifeq ($(HB_CMP),)
   ifeq ($(HB_BUILD_MODE),cpp)
      HB_CMP := g++
   else
      HB_CMP := gcc
   endif
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

HB_DYN_COPT := -DHB_DYNLIB -fPIC

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
# NOTE: The ending space after -o is important, please preserve it.
CC_OUT := -o$(subst x,x, )

CFLAGS += -I. -I$(HB_HOST_INC)

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

LD := $(CC)
LD_OUT := -o

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPREFIX)ar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) -Wl,-soname,$(DYN_NAME_CPT) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)

include $(TOP)$(ROOT)config/rules.mk
