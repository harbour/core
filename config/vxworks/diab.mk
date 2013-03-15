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
else
ifeq ($(HB_CPU),arm)
   _DIAB_CPU := ARMV7LS
else
ifeq ($(HB_CPU),mips)
   _DIAB_CPU :=
else
ifeq ($(HB_CPU),ppc)
   _DIAB_CPU :=
else
ifeq ($(HB_CPU),sh)
   _DIAB_CPU :=
else
ifeq ($(HB_CPU),m68k)
   _DIAB_CPU :=
endif
endif
endif
endif
endif
endif

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)
CC_IN := -c
CC_OUT := -o

CFLAGS += -t$(_DIAB_CPU):rtp -WDVSB_DIR=$(WIND_BASE)/target/lib
LDFLAGS += -t$(_DIAB_CPU):rtp -WDVSB_DIR=$(WIND_BASE)/target/lib
DFLAGS += -t$(_DIAB_CPU):rtp -WDVSB_DIR=$(WIND_BASE)/target/lib

CFLAGS += -I. -I$(HB_HOST_INC)
CFLAGS += -I$(WIND_BASE)/target/usr/h
CFLAGS += -I$(WIND_BASE)/target/usr/h/wrn/coreip
CFLAGS += -D_VX_CPU=$(_HB_VXCPU)
#CFLAGS += -D_VX_TOOL_FAMILY=diab -D_VX_TOOL=diab

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

ifeq ($(HB_BUILD_SHARED),yes)
   LDFLAGS += -Xdynamic -Wl,-rpath /romfs/lib
endif
LDFLAGS += $(LDLIBPATHS)

AR := $(HB_CCPREFIX)dar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -Xpic -Wl, -Xshared -Wl, -Xdynamic $(DLIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS_DYN),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) -soname="$(DYN_NAME_CPT)" $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ -@__dyn__.tmp $(DLIBS) $(DYSTRIP)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
