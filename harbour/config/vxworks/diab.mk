#
# $Id$
#

ifeq ($(HB_CMP),)
   ifeq ($(HB_BUILD_MODE),cpp)
      HB_CMP := dplus
   else
      HB_CMP := dcc
   endif
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

HB_DYN_COPT := -DHB_DYNLIB -Xcode-relative-far

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_INC_COMPILE)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Xlint
else
   CFLAGS += -W
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -XO level-3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPREFIX)ar$(HB_CCPOSTFIX)
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE2)

include $(TOP)$(ROOT)config/rules.mk
