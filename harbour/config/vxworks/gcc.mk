#
# $Id$
#

ifeq ($(HB_CMP),)
   ifeq ($(HB_BUILD_MODE),cpp)
      HB_CMP := g++
   else
      HB_CMP := cc
   endif
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

HB_DYN_COPT := -DHB_DYNLIB -fpic

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -mrtp
LDFLAGS += -mrtp
DFLAGS += -mrtp

CFLAGS += -I. -I$(HB_HOST_INC)
CFLAGS += -I$(WIND_BASE)/target/usr/h
CFLAGS += -I$(WIND_BASE)/target/usr/h/wrn/coreip
CFLAGS += -fno-strict-aliasing
CFLAGS += -D_C99 -D_HAS_C9X
#CFLAGS += -D_VX_CPU=$(_HB_VXCPU)
#CFLAGS += -D_VX_TOOL_FAMILY=gnu -D_VX_TOOL=gnu

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

LDLIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS_BIN),-L$(dir))
DLIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS_DYN),-L$(dir))

LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS_BIN),-l$(lib))

ifeq ($(HB_BUILD_SHARED),yes)
   LDFLAGS += -non-static -Wl,-rpath=/romfs/lib
endif
LDFLAGS += $(LDLIBPATHS)

AR := $(HB_CCPREFIX)ar$(HB_CCPOSTFIX)
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(DLIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS_DYN),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) -Wl,-soname,"$(DYN_NAME_CPT)" $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DLIBS) $(DYSTRIP)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
