#
# $Id$
#

# GNU Make file for Open Watcom C/C++ compiler

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

ifeq ($(HB_BUILD_MODE),c)
   CC := wcc386
endif
ifeq ($(HB_BUILD_MODE),cpp)
   CC := wpp386
endif
# Build in C++ mode by default
ifeq ($(HB_BUILD_MODE),)
   CC := wpp386
endif
CC_IN :=
CC_OUT := -fo=

CPPFLAGS := -zq -bt=dos
CFLAGS :=
LDFLAGS := OP quiet

ifneq ($(HB_BUILD_WARN),no)
   CPPFLAGS += -w3
else
   CPPFLAGS += -w0
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # architecture flags
   CPPFLAGS += -5r -fp5

   # optimization flags
   # don't enable -ol optimization in OpenWatcom 1.1 - gives buggy code
   # -oxaht
   CPPFLAGS += -onaehtr -s -ei -zp4 -zt0
   #CPPFLAGS += -obl+m
   ifeq ($(CC),wpp386)
      CPPFLAGS += -oi+
   else
      CPPFLAGS += -oi
   endif
endif

CPPFLAGS += -i. -i$(HB_INC_COMPILE)

ifeq ($(HB_BUILD_DEBUG),yes)
   CPPFLAGS += -d2
endif

LD := wlink
ifeq ($(HB_BUILD_DEBUG),yes)
   LDFLAGS += DEBUG ALL
endif
# different SYS values: dos4g (default), pmodew (commercial), causeway
ifeq ($(LIBNAME),hbpp)
   # we force causeway here as workaround for reduced command line size in dos4g
   LDFLAGS += SYS causeway
else
   LDFLAGS += SYS dos4g OP stub=wstubq.exe
endif

LDLIBS := $(HB_USER_LIBS)
LDLIBS += $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))

ifneq ($(HB_LINKING_RTL),)
   ifneq ($(HB_HAS_WATT),)
      LDLIBS += $(HB_LIB_WATT)/wattcpwf
   endif
endif

DY := $(LD)
DFLAGS := OP quiet SYS cwdllr
DY_OUT :=
DLIBS := $(foreach lib,$(HB_USER_LIBS),$(lib))
DLIBS += $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))
DLIBS += $(foreach lib,$(SYSLIBS),$(lib))
DLIBS := $(strip $(DLIBS))

ifneq ($(DLIBS),)
   comma := ,
   DLIBS_COMMA := LIB $(subst $(subst x,x, ),$(comma) ,$(DLIBS))
else
   DLIBS_COMMA :=
endif

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)FILE '$(file)'$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) NAME '$(subst /,$(DIRSEP),$(DYN_DIR)/$@)' OP implib='$(IMP_FILE)' @__dyn__.tmp $(DLIBS_COMMA)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/common/watcom.mk
