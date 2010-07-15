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

HB_DYN_COPT := -DHB_DYNLIB -Xpic

ifeq ($(HB_CPU),x86)
   _DIAB_CPU := X86LH
   _DIAB_VXCPU := _VX_SIMPENTIUM
else
ifeq ($(HB_CPU),arm)
   _DIAB_CPU :=
   _DIAB_VXCPU :=
else
ifeq ($(HB_CPU),mips)
   _DIAB_CPU :=
   _DIAB_VXCPU :=
else
ifeq ($(HB_CPU),ppc)
   _DIAB_CPU :=
   _DIAB_VXCPU :=
else
ifeq ($(HB_CPU),sh)
   _DIAB_CPU :=
   _DIAB_VXCPU :=
else
ifeq ($(HB_CPU),m68k)
   _DIAB_CPU :=
   _DIAB_VXCPU :=
endif
endif
endif
endif
endif
endif

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_INC_COMPILE)
CFLAGS += -I$(WIND_BASE)/target/h -I$(WIND_BASE)/target/h/wrn/coreip

CFLAGS += -D_VX_CPU=$(_DIAB_VXCPU)

CFLAGS += -t$(_DIAB_CPU):rtpsim -WDVSB_DIR=$(WIND_BASE)/target/lib
LDFLAGS += -t$(_DIAB_CPU):rtpsim -WDVSB_DIR=$(WIND_BASE)/target/lib
DFLAGS += -t$(_DIAB_CPU):rtpsim -WDVSB_DIR=$(WIND_BASE)/target/lib

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

LD := $(CC)
LD_OUT := -o

LDLIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS_BIN),-L$(dir))
DLIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS_DYN),-L$(dir))

LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS_BIN),-l$(lib))

LDFLAGS += $(LDLIBPATHS)

AR := $(HB_CCPREFIX)dar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(DLIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS_DYN),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE2)

include $(TOP)$(ROOT)config/rules.mk
