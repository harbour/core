ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

HB_DYN_COPT := -DHB_DYNLIB -fPIC

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

# uncomment this if you want to force creating 64bit binaries
#CFLAGS += -maix64
#LDFLAGS += -maix64 -Wl,-b64
#DFLAGS += -maix64 -Wl,-b64
#ARFLAGS += -X64

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

LDFLAGS += $(LIBPATHS) # -Wl,-bnoquiet

# uncomment following block for AIX linker workaround (linking all symbols issue)
# order of libraries is more important this way

define aix_ld_hblib
   $(LD) -Wl,-r,-bgc,-bkeepfile:__applnk__.o -nostartfiles $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) -o__apptmp__.o __applnk__.o -l$(lib)
   mv __apptmp__.o __applnk__.o

endef

define aix_ld
   $(LD) -Wl,-r $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) -o__apptmp__.o $(^F)
   mv __apptmp__.o __applnk__.o
   $(foreach lib,$(HB_USER_LIBS) $(LIBS),$(aix_ld_hblib))
   $(LD) -nostartfiles $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) $(LD_OUT)$(subst /,$(DIRSEP),$(BIN_DIR)/$@) __applnk__.o $(foreach lib,$(SYSLIBS),-l$(lib)) $(LDSTRIP)
   $(RM) __applnk__.o
endef

LD_RULE = $(aix_ld)

# end of workaround block

AR := $(HB_CCPREFIX)ar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rc $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -shared -Wl,-G $(LIBPATHS)
# TOFIX: CHECKME, there was space between -o and output name
#DY_OUT := -o$(subst x,x, )
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
#define dynlib_object
#   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

#endef
#define create_dynlib
#   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
#   $(foreach file,$^,$(dynlib_object))
#   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)
#endef

#DY_RULE = $(create_dynlib)

DY_RULE = $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)

include $(TOP)$(ROOT)config/rules.mk
