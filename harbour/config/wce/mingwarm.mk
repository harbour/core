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

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_INC_COMPILE)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # Use -O2 instead of -O3 here.
   CFLAGS += -O2
   ifneq ($(HB_BUILD_DEBUG),yes)
      CFLAGS += -fomit-frame-pointer
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

RC := $(HB_CCPATH)$(HB_CCPREFIX)windres
RC_OUT := -o$(subst x,x, )
RCFLAGS := -O coff

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && ${HB_CCPATH}${HB_CCPREFIX}strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif

LD := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o$(subst x,x, )

LIBPATHS := -L$(LIB_DIR)
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPATH)$(HB_CCPREFIX)ar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@)

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DLIBS) -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def $(DYSTRIP)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
